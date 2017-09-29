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
        case Tessla.Def(name, Seq(), _, _, loc) => Tessla.Out(Tessla.ExprName(name), None, loc)
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
        case definition@Tessla.Def(name, args, _, _, loc) =>
          previousDefs.get((definition.name.name, args.length)) match {
            case Some(previousLoc) =>
              error(MultipleDefinitionsError(definition.name, previousLoc))
              None
            case None =>
              previousDefs += (name.name, args.length) -> loc
              val uniqueDef = definition.copy(name = definition.name.copy(name = mkId(definition.name.name)))
              Some((name.name, definition.macroArgs.length) -> Definition(uniqueDef, env))
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

    def translateExpression(expr: Tessla.Expr, name: String, env: Env): TranslatedExpression = {
      val errorNode: TranslatedExpression = (Seq(), Stream(errorStream, Types.Nothing))
      tryWithDefault(errorNode) {
        if (alreadyTranslated.contains(name)) (Seq(), alreadyTranslated(name))
        else {
          // This value will be overridden later. This one will only be reached in case of recursion, in which case
          // it should be the correct one.
          alreadyTranslated(name) = Stream(TesslaCore.Stream(name, UnknownLoc), Types.Nothing)
          val (defs, arg): TranslatedExpression = expr match {
            case Tessla.ExprBoolLit(value, loc) =>
              (Seq(), Literal(TesslaCore.BoolLiteral(value, loc)))
            case Tessla.ExprIntLit(value, loc) =>
              (Seq(), Literal(TesslaCore.IntLiteral(value, loc)))
            case Tessla.ExprTimeLit(value, unit2, loc) => unit match {
              case Some(u) => if (unit2 < u) {
                throw TimeUnitConversionError(unit2, u)
              } else {
                (Seq(), Literal(TesslaCore.IntLiteral(value * unit2.convertTo(u), loc)))
              }
              case None => throw UndefinedTimeUnit(loc)
            }
            case Tessla.ExprUnit(loc) =>
              (Seq(), Literal(TesslaCore.Unit(loc)))
            case Tessla.ExprStringLit(str, loc) =>
              (Seq(), Literal(TesslaCore.StringLiteral(str, loc)))
            case Tessla.ExprName(id) =>
              env.get((id.name, 0)) match {
                case Some(Definition(d, closure)) =>
                  updateLoc(translateExpression(d.body, d.name.name, closure), id.loc)
                case Some(BuiltIn(b)) =>
                  b(Seq(), name, id.loc)
                case None =>
                  inStreams.get(id.name) match {
                    case Some((typ, _)) =>
                      (Seq(), Stream(TesslaCore.InputStream(id.name, id.loc), typ.elementType))
                    case None => throw UndefinedVariable(id)
                  }
              }
            case Tessla.ExprTypeAscr(e, t) =>
              alreadyTranslated.remove(name)
              val (statements, arg) = translateExpression(e, name, env)
              Types.requireType(Types.fromAst(t), arg.typ, e.loc)
              (statements, arg)
            case Tessla.ExprBlock(definitions, expression, _) =>
              lazy val scope: Env = env ++ mkEnv(definitions, scope)
              alreadyTranslated.remove(name)
              translateExpression(expression, name, scope)
            case Tessla.ExprApp(id, args, loc) =>
              env.get((id.name, args.length)) match {
                case Some(Definition(d, closure)) =>
                  val namedArgs: Env = args.collect {
                    case Tessla.NamedArg(argName, expr) =>
                      d.macroArgs.find(_.name.name == argName.name) match {
                        case Some(arg) =>
                          val typedExpr = arg.typeAscr.map(t => Tessla.ExprTypeAscr(expr, t.withLoc(arg.loc))).getOrElse(expr)
                          val newName = Tessla.Identifier(mkId(argName.name), argName.loc)
                          (argName.name, 0) -> Definition(Tessla.Def(newName, Seq(), None, typedExpr, argName.loc), env)
                        case None =>
                          throw UndefinedNamedArg(argName)
                      }
                  }.toMap
                  val posArgs: Env = d.macroArgs.filterNot { arg =>
                    namedArgs.contains((arg.name.name, 0))
                  }.zip(args.collect { case Tessla.PosArg(expr) => expr }).map {
                    case (arg, expr) =>
                      val typedExpr = arg.typeAscr.map(t => Tessla.ExprTypeAscr(expr, t.withLoc(arg.loc))).getOrElse(expr)
                      val newName = Tessla.Identifier(mkId(arg.name.name), arg.name.loc)
                      (arg.name.name, 0) -> Definition(Tessla.Def(newName, Seq(), None, typedExpr, arg.name.loc), env)
                  }.toMap
                  alreadyTranslated.remove(name)
                  val result@(_, resultExp) = updateLoc(translateExpression(d.body, name, closure ++ posArgs ++ namedArgs), loc)
                  d.typeAscr.foreach { t =>
                    Types.requireType(Types.fromAst(t), resultExp.typ, resultExp.loc)
                  }
                  result
                case Some(BuiltIn(b)) =>
                  val coreArgs = args.map {
                    case Tessla.PosArg(expr) => translateExpression(expr, mkId(name), env)
                    case Tessla.NamedArg(argName, _) => throw UndefinedNamedArg(argName)
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
        translateExpression(out.expr, out.nameOpt.map(_.name).getOrElse(outName), globalEnv) match {
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

  class Definition(val definition: Tessla.Def, _closure: => Env) extends EnvEntry {
    lazy val closure = _closure
  }

  object Definition {
    def apply(definition: Tessla.Def, _closure: => Env) = new Definition(definition, _closure)

    def unapply(d: Definition): Option[(Tessla.Def, Env)] = Some((d.definition, d.closure))
  }

  type Env = Map[(String, Int), EnvEntry]
}
