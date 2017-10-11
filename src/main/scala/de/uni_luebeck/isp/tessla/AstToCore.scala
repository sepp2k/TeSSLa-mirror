package de.uni_luebeck.isp.tessla

import AstToCore._
import de.uni_luebeck.isp.tessla.Errors._

import scala.collection.mutable

class AstToCore(val unit: Option[TimeUnit.TimeUnit]) extends TranslationPhase[Tessla.Spec, TesslaCore.Specification] {
  override def translateSpec(spec: Tessla.Spec) = {
    var counter = 0
    val alreadyTranslated = mutable.Map[String, Arg]()

    val inStreams = spec.statements.collect {
      case Tessla.In(name, typAst, loc) =>
        val typ = tryWithDefault(Types.Stream(Types.Nothing)) {
          Types.fromAst(typAst) match {
            case s@Types.Stream(_) => s
            case t => throw TypeMismatch(Types.Stream(Types.Nothing), t, typAst.loc)
          }
        }
        name.name -> (typ, loc)
    }.toMap

    val outStreams = spec.statements.collect {
      case out@Tessla.Out(_, _, _) => Seq(out)
      case Tessla.OutAll(_) => spec.statements.collect {
        case Tessla.Definition(name, Seq(), _, _, loc) => Tessla.Out(Tessla.Variable(name), None, loc)
      }
    }.flatten : Seq[Tessla.Out]


    def mkId(name: String) = {
      counter += 1
      name.replaceFirst("\\$\\d+$", "") + "$" + counter
    }

    val builtIns = BuiltIns(mkId)

    def mkEnv(statements: Seq[Tessla.Statement], env: => Env) = {
      val previousDefs = mutable.Map[(String, Int), Location]()
      statements.flatMap {
        case definition@Tessla.Definition(name, args, _, _, loc) =>
          previousDefs.get((definition.id.name, args.length)) match {
            case Some(previousLoc) =>
              error(MultipleDefinitionsError(definition.id, previousLoc))
              None
            case None =>
              previousDefs += (name.name, args.length) -> loc
              val uniqueDef = definition.copy(id = definition.id.copy(name = mkId(definition.id.name)))
              Some((name.name, definition.parameters.length) -> Definition(uniqueDef, env))
          }
        case _ => None
      }
    }

    lazy val globalEnv: Env = builtIns.map {
      case (key, b) => key -> BuiltIn(b)
    } ++ mkEnv(spec.statements, globalEnv)

    def updateLoc(exp: TranslatedExpression, loc: Location): TranslatedExpression = {
      (exp._1, exp._2.withLoc(loc))
    }

    val errorStream: TesslaCore.StreamRef = TesslaCore.Stream("$$$error$$$", UnknownLoc)

    def translateExpression(expr: Tessla.Expression, name: String, env: Env): TranslatedExpression = {
      val errorNode: TranslatedExpression = (Seq(), Stream(errorStream, Types.Nothing))
      tryWithDefault(errorNode) {
        if (alreadyTranslated.contains(name)) (Seq(), alreadyTranslated(name))
        else {
          // This value will be overridden later. This one will only be reached in case of recursion, in which case
          // it should be the correct one.
          alreadyTranslated(name) = Stream(TesslaCore.Stream(name, UnknownLoc), Types.Nothing)
          val (defs, arg): TranslatedExpression = expr match {
            case Tessla.BoolLiteral(value, loc) =>
              (Seq(), Literal(TesslaCore.BoolLiteral(value, loc)))
            case Tessla.IntLiteral(value, loc) =>
              (Seq(), Literal(TesslaCore.IntLiteral(value, loc)))
            case Tessla.TimeLiteral(value, unit2, loc) => unit match {
              case Some(u) => if (unit2 < u) {
                throw TimeUnitConversionError(unit2, u)
              } else {
                (Seq(), Literal(TesslaCore.IntLiteral(value * unit2.convertTo(u), loc)))
              }
              case None => throw UndefinedTimeUnit(loc)
            }
            case Tessla.Unit(loc) =>
              (Seq(), Literal(TesslaCore.Unit(loc)))
            case Tessla.StringLiteral(str, loc) =>
              (Seq(), Literal(TesslaCore.StringLiteral(str, loc)))
            case Tessla.Variable(id) =>
              env.get((id.name, 0)) match {
                case Some(Definition(d, closure)) =>
                  updateLoc(translateExpression(d.body, d.id.name, closure), id.loc)
                case Some(BuiltIn(b)) =>
                  b(Seq(), name, id.loc)
                case None =>
                  inStreams.get(id.name) match {
                    case Some((typ, _)) =>
                      (Seq(), Stream(TesslaCore.InputStream(id.name, id.loc), typ.elementType))
                    case None => throw UndefinedVariable(id)
                  }
              }
            case Tessla.TypeAssertion(e, t) =>
              alreadyTranslated.remove(name)
              val (statements, arg) = translateExpression(e, name, env)
              Types.requireType(Types.fromAst(t), arg.typ, e.loc)
              (statements, arg)
            case Tessla.Block(definitions, expression, _) =>
              lazy val scope: Env = env ++ mkEnv(definitions, scope)
              alreadyTranslated.remove(name)
              translateExpression(expression, name, scope)
            case Tessla.MacroCall(id, args, loc) =>
              env.get((id.name, args.length)) match {
                case Some(Definition(d, closure)) =>
                  val namedArgs: Env = args.collect {
                    case Tessla.NamedArgument(argName, expr) =>
                      d.parameters.find(_.id.name == argName.name) match {
                        case Some(arg) =>
                          val typedExpr = arg.parameterType.map(t => Tessla.TypeAssertion(expr, t.withLoc(arg.loc))).getOrElse(expr)
                          val newName = Tessla.Identifier(mkId(argName.name), argName.loc)
                          (argName.name, 0) -> Definition(Tessla.Definition(newName, Seq(), None, typedExpr, argName.loc), env)
                        case None =>
                          throw UndefinedNamedArg(argName)
                      }
                  }.toMap
                  val posArgs: Env = d.parameters.filterNot { arg =>
                    namedArgs.contains((arg.id.name, 0))
                  }.zip(args.collect { case Tessla.PositionalArgument(expr) => expr }).map {
                    case (arg, expr) =>
                      val typedExpr = arg.parameterType.map(t => Tessla.TypeAssertion(expr, t.withLoc(arg.loc))).getOrElse(expr)
                      val newName = Tessla.Identifier(mkId(arg.id.name), arg.id.loc)
                      (arg.id.name, 0) -> Definition(Tessla.Definition(newName, Seq(), None, typedExpr, arg.id.loc), env)
                  }.toMap
                  alreadyTranslated.remove(name)
                  val result@(_, resultExp) = updateLoc(translateExpression(d.body, name, closure ++ posArgs ++ namedArgs), loc)
                  d.returnType.foreach { t =>
                    Types.requireType(Types.fromAst(t), resultExp.typ, resultExp.loc)
                  }
                  result
                case Some(BuiltIn(b)) =>
                  val coreArgs = args.map {
                    case Tessla.PositionalArgument(expr) => translateExpression(expr, mkId(name), env)
                    case Tessla.NamedArgument(argName, _) => throw UndefinedNamedArg(argName)
                  }
                  val (defs, arg) = b(coreArgs.map(_._2), name, loc)
                  (coreArgs.flatMap(_._1) ++ defs, arg)

                case None => throw UndefinedFunction(id, args.length)
              }
          }
          alreadyTranslated(name) = arg
          (defs, arg)
        }
      }
    }

    val outs: Seq[(Seq[(String, TesslaCore.Expression)], (String, TesslaCore.StreamRef))] = outStreams.map { out =>
      val name = out.name
      val loc = out.expr.loc
      tryWithDefault((Seq[(String, TesslaCore.Expression)](), name -> errorStream)) {
        val outName = mkId("out")
        translateExpression(out.expr, out.idOpt.map(_.name).getOrElse(outName), globalEnv) match {
          case (defs, Stream(s, _)) =>
            (defs, name -> s)
          case (_, lit) =>
            throw TypeMismatch(Types.Stream(Types.Nothing), lit.typ, loc)
        }
      }
    }

    TesslaCore.Specification(outs.flatMap(_._1).toMap, inStreams.map { case (n, (t, l)) => (n, t, l) }.toSeq, outs.map(_._2))
  }
}

object AstToCore {

  sealed abstract class Arg {
    def typ: Types.Type

    def loc: Location

    def withLoc(loc: Location): Arg
  }

  case class Stream(value: TesslaCore.StreamRef, elementType: Types.ValueType) extends Arg {
    def typ: Types.Stream = Types.Stream(elementType)

    def loc = value.loc

    def withLoc(loc: Location): Stream = Stream(value.withLoc(loc), elementType)
  }

  case class Literal(value: TesslaCore.LiteralValue) extends Arg {
    def typ: Types.ValueType = value.typ

    def loc = value.loc

    def withLoc(loc: Location): Literal = Literal(value.withLoc(loc))
  }

  type TranslatedExpression = (Seq[(String, TesslaCore.Expression)], Arg)

  sealed abstract class EnvEntry

  case class BuiltIn(apply: (Seq[Arg], String, Location) => TranslatedExpression) extends EnvEntry

  class Definition(val definition: Tessla.Definition, _closure: => Env) extends EnvEntry {
    lazy val closure = _closure
  }

  object Definition {
    def apply(definition: Tessla.Definition, _closure: => Env) = new Definition(definition, _closure)

    def unapply(d: Definition): Option[(Tessla.Definition, Env)] = Some((d.definition, d.closure))
  }

  type Env = Map[(String, Int), EnvEntry]
}
