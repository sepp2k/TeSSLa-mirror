package de.uni_luebeck.isp.tessla

import AstToCore._

import scala.collection.mutable

class AstToCore extends TranslationPhase[Ast.Spec, TesslaCore.Specification] {
  override def translateSpec(spec: Ast.Spec) = {
    var counter = 0
    val alreadyTranslated = mutable.Map[String, TesslaCore.Arg]()

    val inStreams = spec.statements.collect {
      case Ast.In(name, _, loc) => (name.name, loc)
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
      val previousDefs = mutable.Map[String, Location]()
      statements.flatMap {
        case definition @ Ast.Def(name, _, _, _, loc) =>
          previousDefs.get(definition.name.name) match {
            case Some(previousLoc) =>
              error(MultipleDefinitionsError(definition.name, previousLoc))
              None
            case None =>
              previousDefs += name.name -> loc
              val uniqueDef = definition.copy(name = definition.name.copy(name = mkId(definition.name.name)))
              Some((name.name, definition.macroArgs.length) -> Definition(uniqueDef, env))
          }
        case _ => None
      }
    }

    lazy val globalEnv: Env = builtIns.map {
      case (key, b) => key -> BuiltIn(b)
    } ++ mkEnv(spec.statements, globalEnv)

    val errorStream: TesslaCore.StreamRef = TesslaCore.Stream("$$$error$$$", UnknownLoc)
    def translateExpression(expr: Ast.Expr, name: String, env: Env): TranslatedExpression = {
      val errorNode: TranslatedExpression = (Seq(), errorStream)
      tryWithDefault(errorNode) {
        if (alreadyTranslated.contains(name)) (Seq(), alreadyTranslated(name))
        else {
          // This value will be overridden later. This one will only be reached in case of recursion, in which case
          // it should be the correct one.
          alreadyTranslated(name) = TesslaCore.Stream(name, UnknownLoc)
          val (defs, arg) = expr match {
            case Ast.ExprBoolLit(value, loc) =>
              (Seq(), TesslaCore.BoolLiteral(value, loc))
            case Ast.ExprIntLit(value, loc) =>
              (Seq(), TesslaCore.IntLiteral(value, loc))
            case Ast.ExprUnit(loc) =>
              (Seq(), TesslaCore.Unit(loc))
            case Ast.ExprStringLit(str, loc) =>
              (Seq(), TesslaCore.StringLiteral(str, loc))
            case Ast.ExprName(id) =>
              env.get((id.name, 0)) match {
                case Some(Definition(d, closure)) =>
                  translateExpression(d.definition, d.name.name, closure)
                case Some(BuiltIn(b)) =>
                  b(Seq(), name, id.loc)
                case None =>
                  inStreams.get(id.name) match {
                    case Some(loc) => (Seq(), TesslaCore.InputStream(id.name, loc))
                    case None => throw UndefinedVariable(id)
                  }
              }
            case Ast.ExprTypeAscr(e, _) =>
              alreadyTranslated.remove(name)
              translateExpression(e, name, env)
            case Ast.ExprBlock(definitions, expression, _) =>
              lazy val scope: Env = env ++ mkEnv(definitions, scope)
              alreadyTranslated.remove(name)
              translateExpression(expression, name, scope)
            case Ast.ExprApp(id, args, loc) =>
              env.get((id.name, args.length)) match {
                case Some(Definition(d, closure)) =>
                  val namedArgs: Env = args.collect {
                    case Ast.NamedArg(argName, expr) =>
                      if (d.macroArgs.exists(_.name.name == argName.name)) {
                        val newName = Ast.Identifier(mkId(argName.name), argName.loc)
                        (argName.name, 0) -> Definition(Ast.Def(newName, Seq(), None, expr, argName.loc), env)
                      } else throw UndefinedNamedArg(argName)
                  }.toMap
                  val posArgs: Env = d.macroArgs.filterNot { argName =>
                    namedArgs.contains((argName.name.name, 0))
                  }.zip(args.collect { case Ast.PosArg(expr) => expr }).map {
                    case (argName, expr) =>
                      val newName = Ast.Identifier(mkId(argName.name.name), argName.name.loc)
                      (argName.name.name, 0) -> Definition(Ast.Def(newName, Seq(), None, expr, argName.name.loc), env)
                  }.toMap
                  alreadyTranslated.remove(name)
                  translateExpression(d.definition, name, closure ++ posArgs ++ namedArgs)
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
          case (defs, s: TesslaCore.StreamRef) =>
            (defs, name -> s)
          case (_, _) =>
            throw TypeError("stream", "constant value", loc)
        }
      }
    }

    TesslaCore.Specification(outs.flatMap(_._1).toMap, inStreams.toSeq, outs.map(_._2))
  }
}

object AstToCore {
  type TranslatedExpression = (Seq[(String, TesslaCore.Expression)], TesslaCore.Arg)
  sealed abstract class EnvEntry
  case class BuiltIn(apply: (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression) extends EnvEntry
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

  case class TypeError(expected: String, found: String, loc: Location) extends CompilationError {
    override def message = s"Type mismatch: Expected $expected, found $found."
  }

  case class MultipleDefinitionsError(id: Ast.Identifier, previousLoc: Location) extends CompilationError {
    override def loc = id.loc
    override def message = s"Multiple definitions of ${id.name} in same scope (previous definition at $previousLoc)"
  }
}
