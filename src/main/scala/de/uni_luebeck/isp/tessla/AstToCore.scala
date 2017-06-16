package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import AstToCore._

class AstToCore(spec: Ast.Spec) {
  var counter = 0
  val alreadyTranslated = mutable.Map[String, TesslaCore.Arg]()

  val inStreams = spec.statements.collect {
    case Ast.In(name, _, loc) => (name.name, loc)
  }.toMap

  val outStreams = spec.statements.collect {
    case out @ Ast.Out(_, _) => out
  }

  val globalDefs = spec.statements.collect {
    case definition @ Ast.Def(name, _, _, _, _) => name.name -> definition
  }.toMap

  def mkId(name: String) = {
    counter += 1
    name.replaceFirst("\\$.*$", "") + "$" + counter
  }

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

  val builtins: Map[(String, Int), (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression] = Map(
    ("nil", 0) -> {
      case (Seq(), _, loc) => (Seq(), TesslaCore.Nil(loc))
    },
    ("default", 2) -> {
      case (Seq(stream: TesslaCore.StreamRef, default: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> TesslaCore.DefaultFrom(stream, default, loc)), TesslaCore.Stream(name, loc))
      case (Seq(stream: TesslaCore.StreamRef, default: TesslaCore.LiteralValue), name, loc) =>
        (Seq(name -> TesslaCore.Default(stream, default, loc)), TesslaCore.Stream(name, loc))
      case (Seq(stream, _), _, _) => throw TypeError("stream", "constant value", stream.loc)
    },
    ("last", 2) -> {
      case (Seq(values: TesslaCore.StreamRef, clock: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> TesslaCore.Last(values, clock, loc)), TesslaCore.Stream(name, loc))
      case (Seq(values: TesslaCore.LiteralValue, _), _, _) =>
        throw TypeError("stream", "constant value", values.loc)
      case (Seq(_, clock: TesslaCore.LiteralValue, _), _, _) =>
        throw TypeError("stream", "constant value", clock.loc)
    },
    ("+", 2) -> {
      case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> TesslaCore.Add(lhs, rhs, loc)), TesslaCore.Stream(name, loc))
      case (Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)), name, loc) =>
        (Seq(), TesslaCore.IntLiteral(lhs + rhs, loc))
      case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.LiteralValue), name, loc) =>
        val liftedRhs = mkId(name)
        (
          Seq(
            liftedRhs -> TesslaCore.Default(TesslaCore.Nil(loc), rhs, loc),
            name -> TesslaCore.Add(lhs, TesslaCore.Stream(liftedRhs, loc), loc)
          ),
          TesslaCore.Stream(name, loc)
        )
      case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.StreamRef), name, loc) =>
        val liftedLhs = mkId(name)
        (
          Seq(
            liftedLhs -> TesslaCore.Default(TesslaCore.Nil(loc), lhs, loc),
            name -> TesslaCore.Add(TesslaCore.Stream(liftedLhs, loc), rhs, loc)
          ),
          TesslaCore.Stream(name, loc)
        )
    }
  )

  lazy val globalEnv: Env = builtins.map {
    case (key, b) => key -> BuiltIn(b)
  } ++ globalDefs.map {
    case (name, d) => (name, d.macroArgs.length) -> Definition(d, globalEnv)
  }

  def translateSpec() = {
    val streams: Map[String, TesslaCore.Expression] = spec.statements.flatMap {
      case Ast.Def(name, Seq(), _, expr, _) =>
        translateExpression(expr, name.name, globalEnv)._1
      case _ => Seq()
    }.toMap
    def outs = outStreams.map { out =>
      val name = out.name.name
      val loc = out.name.loc
      alreadyTranslated.get(name) match {
        case Some(s: TesslaCore.StreamRef) =>
          s
        case Some(_) =>
          throw TypeError("stream", "constant value", loc)
        case None =>
          if (inStreams.contains(name)) TesslaCore.InputStream(name, loc)
          else throw UndefinedVariable(out.name)
      }
    }
    TesslaCore.Specification(streams, inStreams.toSeq, outs)
  }

  def translateExpression(expr: Ast.Expr, name: String, env: Env): TranslatedExpression = {
    if (alreadyTranslated.contains(name)) (Seq(), alreadyTranslated(name))
    else {
      // This value will be overridden later. This one will only be reached in case of recursion, in which case
      // it should be the correct one.
      alreadyTranslated(name) = TesslaCore.Stream(name, UnknownLoc)
      val (defs, arg) = expr match {
        case Ast.ExprBoolLit(Ast.BoolLit(value, loc)) =>
          (Seq(), TesslaCore.BoolLiteral(value, loc))
        case Ast.ExprIntLit(Ast.IntLit(value, loc)) =>
          (Seq(), TesslaCore.IntLiteral(value, loc))
        case Ast.ExprUnit(loc) =>
          (Seq(), TesslaCore.Unit(loc))
        case Ast.ExprStringLit(Ast.StringLit(str, loc)) =>
          (Seq(), TesslaCore.StringLiteral(str, loc))
        case Ast.ExprName(id) =>
          env.get((id.name, 0)) match {
            case Some(Definition(d, closure)) =>
              translateExpression(d.definition, id.name, closure)
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
              }.zip(args.collect { case Ast.PosArg(expr) => expr}).map {
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

object AstToCore extends CompilerPass[Ast.Spec, TesslaCore.Specification] {
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

  override def apply(compiler: Compiler, spec: Ast.Spec) = {
    new AstToCore(spec).translateSpec()
  }
}