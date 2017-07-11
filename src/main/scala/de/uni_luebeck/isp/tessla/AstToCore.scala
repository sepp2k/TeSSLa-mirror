package de.uni_luebeck.isp.tessla

import AstToCore._
import de.uni_luebeck.isp.tessla.Types.TypeMismatch

import scala.collection.mutable

class AstToCore extends TranslationPhase[Ast.Spec, TesslaCore.Specification] {
  override def translateSpec(spec: Ast.Spec) = {
    var counter = 0
    val alreadyTranslated = mutable.Map[String, Arg]()

    val inStreams = spec.statements.collect {
      case Ast.In(name, typAst, loc) =>
        val typ = tryWithDefault(Types.Stream(Types.WildCard)) {
          Types.fromAst(typAst) match {
            case s @ Types.Stream(_) => s
            case t => throw TypeMismatch(Types.Stream(Types.WildCard), t, typAst.loc)
          }
        }
        name.name -> (typ, loc)
    }.toMap

    val outStreams = spec.statements.collect {
      case out @ Ast.Out(_, _, _) => out
    }

    def mkId(name: String) = {
      counter += 1
      name.replaceFirst("\\$\\d+$", "") + "$" + counter
    }

    val builtIns = BuiltIns(mkId)

    def mkEnv(statements: Seq[Ast.Statement], env: =>Env) = {
      val previousDefs = mutable.Map[(String, Int), Location]()
      statements.flatMap {
        case definition @ Ast.Def(name, args, _, _, loc) =>
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
    def translateExpression(expr: Ast.Expr, name: String, env: Env): TranslatedExpression = {
      val errorNode: TranslatedExpression = (Seq(), Stream(errorStream, Types.WildCard))
      tryWithDefault(errorNode) {
        if (alreadyTranslated.contains(name)) (Seq(), alreadyTranslated(name))
        else {
          // This value will be overridden later. This one will only be reached in case of recursion, in which case
          // it should be the correct one.
          alreadyTranslated(name) = Stream(TesslaCore.Stream(name, UnknownLoc), Types.WildCard)
          val (defs, arg): TranslatedExpression = expr match {
            case Ast.ExprBoolLit(value, loc) =>
              (Seq(), Literal(TesslaCore.BoolLiteral(value, loc)))
            case Ast.ExprIntLit(value, loc) =>
              (Seq(), Literal(TesslaCore.IntLiteral(value, loc)))
            case Ast.ExprUnit(loc) =>
              (Seq(), Literal(TesslaCore.Unit(loc)))
            case Ast.ExprStringLit(str, loc) =>
              (Seq(), Literal(TesslaCore.StringLiteral(str, loc)))
            case Ast.ExprName(id) =>
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
            case Ast.ExprTypeAscr(e, t) =>
              alreadyTranslated.remove(name)
              val (statements, arg) = translateExpression(e, name, env)
              Types.requireType(Types.fromAst(t), arg.typ, e.loc)
              (statements, arg)
            case Ast.ExprBlock(definitions, expression, _) =>
              lazy val scope: Env = env ++ mkEnv(definitions, scope)
              alreadyTranslated.remove(name)
              translateExpression(expression, name, scope)
            case Ast.ExprApp(id, args, loc) =>
              env.get((id.name, args.length)) match {
                case Some(Definition(d, closure)) =>
                  val namedArgs: Env = args.collect {
                    case Ast.NamedArg(argName, expr) =>
                      d.macroArgs.find(_.name.name == argName.name) match {
                        case Some(arg) =>
                          val typedExpr = arg.typeAscr.map(t => Ast.ExprTypeAscr(expr, t.withLoc(arg.loc))).getOrElse(expr)
                          val newName = Ast.Identifier(mkId(argName.name), argName.loc)
                          (argName.name, 0) -> Definition(Ast.Def(newName, Seq(), None, typedExpr, argName.loc), env)
                        case None =>
                          throw UndefinedNamedArg(argName)
                      }
                  }.toMap
                  val posArgs: Env = d.macroArgs.filterNot { arg =>
                    namedArgs.contains((arg.name.name, 0))
                  }.zip(args.collect { case Ast.PosArg(expr) => expr }).map {
                    case (arg, expr) =>
                      val typedExpr = arg.typeAscr.map(t => Ast.ExprTypeAscr(expr, t.withLoc(arg.loc))).getOrElse(expr)
                      val newName = Ast.Identifier(mkId(arg.name.name), arg.name.loc)
                      (arg.name.name, 0) -> Definition(Ast.Def(newName, Seq(), None, typedExpr, arg.name.loc), env)
                  }.toMap
                  alreadyTranslated.remove(name)
                  val result@(_, resultExp) = updateLoc(translateExpression(d.body, name, closure ++ posArgs ++ namedArgs), loc)
                  d.typeAscr.foreach { t =>
                    Types.requireType(Types.fromAst(t), resultExp.typ, resultExp.loc)
                  }
                  result
                case Some(BuiltIn(b)) =>
                  val coreArgs = args.map {
                    case Ast.PosArg(expr) => translateExpression(expr, mkId(name), env)
                    case Ast.NamedArg(argName, _) => throw UndefinedNamedArg(argName)
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
            throw TypeMismatch(Types.Stream(Types.WildCard), lit.typ, loc)
        }
      }
    }

    TesslaCore.Specification(outs.flatMap(_._1).toMap, inStreams.map {case (n, (t, l)) => (n,t,l)}.toSeq, outs.map(_._2))
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
    def typ: Types.ValueType = value match {
      case TesslaCore.StringLiteral(_, _) => Types.String
      case TesslaCore.IntLiteral(_, _) => Types.Int
      case TesslaCore.BoolLiteral(_, _) => Types.Bool
      case TesslaCore.Unit(_) => Types.Unit
    }
    def loc = value.loc
    def withLoc(loc: Location): Literal = Literal(value.withLoc(loc))
  }

  type TranslatedExpression = (Seq[(String, TesslaCore.Expression)], Arg)
  sealed abstract class EnvEntry
  case class BuiltIn(apply: (Seq[Arg], String, Location) => TranslatedExpression) extends EnvEntry
  class Definition(val definition: Ast.Def, _closure: =>Env) extends EnvEntry {
    lazy val closure = _closure
  }

  object Definition {
    def apply(definition: Ast.Def, _closure: => Env) = new Definition(definition, _closure)
    def unapply(d: Definition): Option[(Ast.Def, Env)] = Some((d.definition, d.closure))
  }

  type Env = Map[(String, Int), EnvEntry]

  case class InfiniteRecursion(loc: Location) extends CompilationError {
    override def message = "Definition is infinitely recursive"
  }
  case class UndefinedVariable(id: Ast.Identifier) extends CompilationError {
    override def loc = id.loc
    override def message = s"Undefined variable ${id.name}"
  }

  case class UndefinedFunction(id: Ast.Identifier, arity: Int) extends CompilationError {
    override def loc = id.loc
    override def message = s"Undefined macro or operator ${id.name}/$arity"
  }

  case class UndefinedNamedArg(id: Ast.Identifier) extends CompilationError {
    override def loc = id.loc
    override def message = s"Undefined keyword argument ${id.name}"
  }

  case class MultipleDefinitionsError(id: Ast.Identifier, previousLoc: Location) extends CompilationError {
    override def loc = id.loc
    override def message = s"Multiple definitions of ${id.name} in same scope (previous definition at $previousLoc)"
  }

  case class InternalError(m: String, loc: Location = UnknownLoc) {
    def message = s"Internal error: $m"
  }
}
