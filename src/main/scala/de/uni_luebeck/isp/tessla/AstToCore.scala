package de.uni_luebeck.isp.tessla

import AstToCore._

import scala.collection.mutable

class AstToCore extends TranslationPhase[Ast.Spec, TesslaCore.Specification] {
  override def name = "Flatten, macro expand and constant fold"

  override def translateSpec(spec: Ast.Spec) = {
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

    def binaryIntFunction(
                           constantOperation: (BigInt, BigInt, Location) => TesslaCore.LiteralValue,
                           createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
    (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression = {
      case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> createExpression(lhs, rhs, loc)), TesslaCore.Stream(name, loc))
      case (Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)), _, loc) =>
        (Seq(), constantOperation(lhs, rhs, loc))
      case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.LiteralValue), name, loc) =>
        val liftedRhs = mkId(name)
        (
          Seq(
            liftedRhs -> TesslaCore.Default(TesslaCore.Nil(loc), rhs, loc),
            name -> createExpression(lhs, TesslaCore.Stream(liftedRhs, loc), loc)
          ),
          TesslaCore.Stream(name, loc)
        )
      case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.StreamRef), name, loc) =>
        val liftedLhs = mkId(name)
        (
          Seq(
            liftedLhs -> TesslaCore.Default(TesslaCore.Nil(loc), lhs, loc),
            name -> createExpression(TesslaCore.Stream(liftedLhs, loc), rhs, loc)
          ),
          TesslaCore.Stream(name, loc)
        )
      case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.IntLiteral), _, _) =>
        throw TypeError(rhs.getClass.getSimpleName, lhs.getClass.getSimpleName, lhs.loc)
      case (Seq(lhs: TesslaCore.IntLiteral, rhs: TesslaCore.LiteralValue), _, _) =>
        throw TypeError(lhs.getClass.getSimpleName, rhs.getClass.getSimpleName, rhs.loc)
    }

    def binaryBoolFunction(
                            constantOperation: (Boolean, Boolean, Location) => TesslaCore.LiteralValue,
                            createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
    (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression = {
      case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> createExpression(lhs, rhs, loc)), TesslaCore.Stream(name, loc))
      case (Seq(TesslaCore.BoolLiteral(lhs, _), TesslaCore.BoolLiteral(rhs, _)), _, loc) =>
        (Seq(), constantOperation(lhs, rhs, loc))
      case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.LiteralValue), name, loc) =>
        val liftedRhs = mkId(name)
        (
          Seq(
            liftedRhs -> TesslaCore.Default(TesslaCore.Nil(loc), rhs, loc),
            name -> createExpression(lhs, TesslaCore.Stream(liftedRhs, loc), loc)
          ),
          TesslaCore.Stream(name, loc)
        )
      case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.StreamRef), name, loc) =>
        val liftedLhs = mkId(name)
        (
          Seq(
            liftedLhs -> TesslaCore.Default(TesslaCore.Nil(loc), lhs, loc),
            name -> createExpression(TesslaCore.Stream(liftedLhs, loc), rhs, loc)
          ),
          TesslaCore.Stream(name, loc)
        )
      case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.BoolLiteral), _, _) =>
        throw TypeError(rhs.getClass.getSimpleName, lhs.getClass.getSimpleName, lhs.loc)
      case (Seq(lhs: TesslaCore.IntLiteral, rhs: TesslaCore.LiteralValue), _, _) =>
        throw TypeError(lhs.getClass.getSimpleName, rhs.getClass.getSimpleName, rhs.loc)
    }

    def binaryFunction(
                        constantOperation: (Any, Any, Location) => TesslaCore.LiteralValue,
                        createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
    (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression = {
      case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> createExpression(lhs, rhs, loc)), TesslaCore.Stream(name, loc))
      case (Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)), _, loc) =>
        (Seq(), constantOperation(lhs, rhs, loc))
      case (Seq(TesslaCore.BoolLiteral(lhs, _), TesslaCore.BoolLiteral(rhs, _)), _, loc) =>
        (Seq(), constantOperation(lhs, rhs, loc))
      case (Seq(TesslaCore.StringLiteral(lhs, _), TesslaCore.StringLiteral(rhs, _)), _, loc) =>
        (Seq(), constantOperation(lhs, rhs, loc))
      case (Seq(TesslaCore.Unit(_), TesslaCore.Unit(_)), _, loc) =>
        (Seq(), constantOperation((), (), loc))
      case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.LiteralValue), name, loc) =>
        val liftedRhs = mkId(name)
        (
          Seq(
            liftedRhs -> TesslaCore.Default(TesslaCore.Nil(loc), rhs, loc),
            name -> createExpression(lhs, TesslaCore.Stream(liftedRhs, loc), loc)
          ),
          TesslaCore.Stream(name, loc)
        )
      case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.StreamRef), name, loc) =>
        val liftedLhs = mkId(name)
        (
          Seq(
            liftedLhs -> TesslaCore.Default(TesslaCore.Nil(loc), lhs, loc),
            name -> createExpression(TesslaCore.Stream(liftedLhs, loc), rhs, loc)
          ),
          TesslaCore.Stream(name, loc)
        )
      case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.LiteralValue), _, _) =>
        throw TypeError(lhs.getClass.getSimpleName, rhs.getClass.getSimpleName, rhs.loc)
    }

    def unaryIntFunction(
                          constantOperation: (BigInt, Location) => TesslaCore.LiteralValue,
                          createExpression: (TesslaCore.StreamRef, Location) => TesslaCore.Expression):
    (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression = {
      case (Seq(operand: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> createExpression(operand, loc)), TesslaCore.Stream(name, loc))
      case (Seq(TesslaCore.IntLiteral(operand, _)), _, loc) =>
        (Seq(), constantOperation(operand, loc))
      case (Seq(operand: TesslaCore.LiteralValue), _, _) =>
        throw TypeError(classOf[TesslaCore.IntLiteral].getSimpleName, operand.getClass.getSimpleName, operand.loc)
    }

    def unaryBoolFunction(
                           constantOperation: (Boolean, Location) => TesslaCore.LiteralValue,
                           createExpression: (TesslaCore.StreamRef, Location) => TesslaCore.Expression):
    (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression = {
      case (Seq(operand: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> createExpression(operand, loc)), TesslaCore.Stream(name, loc))
      case (Seq(TesslaCore.BoolLiteral(operand, _)), _, loc) =>
        (Seq(), constantOperation(operand, loc))
      case (Seq(operand: TesslaCore.LiteralValue), _, _) =>
        throw TypeError(classOf[TesslaCore.IntLiteral].getSimpleName, operand.getClass.getSimpleName, operand.loc)
    }

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
      ("const", 2) -> {
        case (Seq(value: TesslaCore.LiteralValue, clock: TesslaCore.StreamRef), name, loc) =>
          (Seq(name -> TesslaCore.Const(value, clock, loc)), TesslaCore.Stream(name, loc))
        case (Seq(_: TesslaCore.LiteralValue, clock: TesslaCore.LiteralValue), _, _) =>
          throw TypeError("stream", "constant value", clock.loc)
      },
      ("time", 1) -> {
        case (Seq(clock: TesslaCore.StreamRef), name, loc) =>
          (Seq(name -> TesslaCore.Time(clock, loc)), TesslaCore.Stream(name, loc))
        case (Seq(clock: TesslaCore.LiteralValue), _, _) =>
          throw TypeError("stream", "constant value", clock.loc)
      },
      ("last", 2) -> {
        case (Seq(values: TesslaCore.StreamRef, clock: TesslaCore.StreamRef), name, loc) =>
          (Seq(name -> TesslaCore.Last(values, clock, loc)), TesslaCore.Stream(name, loc))
        case (Seq(values: TesslaCore.LiteralValue, _), _, _) =>
          throw TypeError("stream", "constant value", values.loc)
        case (Seq(_, clock: TesslaCore.LiteralValue, _), _, _) =>
          throw TypeError("stream", "constant value", clock.loc)
      },
      ("delayedLast", 2) -> {
        case (Seq(values: TesslaCore.StreamRef, delays: TesslaCore.StreamRef), name, loc) =>
          (Seq(name -> TesslaCore.DelayedLast(values, delays, loc)), TesslaCore.Stream(name, loc))
        case (Seq(values: TesslaCore.LiteralValue, _), _, _) =>
          throw TypeError("stream", "constant value", values.loc)
        case (Seq(_, delays: TesslaCore.LiteralValue, _), _, _) =>
          throw TypeError("stream", "constant value", delays.loc)
      },
      ("+", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a+b, loc), TesslaCore.Add),
      ("-", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a-b, loc), TesslaCore.Sub),
      ("*", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a*b, loc), TesslaCore.Mul),
      ("&", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a&b, loc), TesslaCore.BitAnd),
      ("|", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a|b, loc), TesslaCore.BitOr),
      ("^", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a^b, loc), TesslaCore.BitXor),
      ("<<", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a << b.toInt, loc), TesslaCore.LeftShift),
      (">>", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a >> b.toInt, loc), TesslaCore.RightShift),
      ("~", 1) -> unaryIntFunction((a,loc) => TesslaCore.IntLiteral(~a, loc), TesslaCore.BitFlip),
      ("<", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.BoolLiteral(a<b, loc), TesslaCore.Lt),
      (">", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.BoolLiteral(a>b, loc), TesslaCore.Gt),
      ("<=", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.BoolLiteral(a<=b, loc), TesslaCore.Lte),
      (">=", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.BoolLiteral(a>=b, loc), TesslaCore.Gte),
      ("==", 2) -> binaryFunction((a,b,loc) => TesslaCore.BoolLiteral(a==b, loc), TesslaCore.Eq),
      ("!=", 2) -> binaryFunction((a,b,loc) => TesslaCore.BoolLiteral(a!=b, loc), TesslaCore.Neq),
      ("&&", 2) -> binaryBoolFunction((a,b,loc) => TesslaCore.BoolLiteral(a&&b, loc), TesslaCore.And),
      ("||", 2) -> binaryBoolFunction((a,b,loc) => TesslaCore.BoolLiteral(a||b, loc), TesslaCore.Or),
      ("!", 1) -> unaryBoolFunction((a,loc) => TesslaCore.BoolLiteral(!a, loc), TesslaCore.Not),
      ("if then else", 3) -> {
        case (Seq(condition: TesslaCore.StreamRef, thenCase: TesslaCore.StreamRef, elseCase: TesslaCore.StreamRef), name, loc) =>
          (Seq(name -> TesslaCore.IfThenElse(condition, thenCase, elseCase, loc)), TesslaCore.Stream(name, loc))
        case (Seq(condition: TesslaCore.StreamRef, thenCase: TesslaCore.LiteralValue, elseCase: TesslaCore.StreamRef), name, loc) =>
          val liftedThenCase = mkId(name)
          (
            Seq(
              liftedThenCase -> TesslaCore.Default(TesslaCore.Nil(loc), thenCase, loc),
              name -> TesslaCore.IfThenElse(condition, TesslaCore.Stream(liftedThenCase, loc), elseCase, loc)
            ),
            TesslaCore.Stream(name, loc)
          )
        case (Seq(condition: TesslaCore.StreamRef, thenCase: TesslaCore.StreamRef, elseCase: TesslaCore.LiteralValue), name, loc) =>
          val liftedElseCase = mkId(name)
          (
            Seq(
              liftedElseCase -> TesslaCore.Default(TesslaCore.Nil(loc), elseCase, loc),
              name -> TesslaCore.IfThenElse(condition, thenCase, TesslaCore.Stream(liftedElseCase, loc), loc)
            ),
            TesslaCore.Stream(name, loc)
          )
        case (Seq(TesslaCore.BoolLiteral(true, _), thenCase: TesslaCore.Arg, _: TesslaCore.Arg), _, _) =>
          (Seq(), thenCase)
        case (Seq(TesslaCore.BoolLiteral(false, _), _: TesslaCore.Arg, elseCase: TesslaCore.Arg), _, _) =>
          (Seq(), elseCase)
        case (Seq(condition: TesslaCore.LiteralValue, _: TesslaCore.Arg, _: TesslaCore.Arg), _, _) =>
          throw TypeError(classOf[TesslaCore.BoolLiteral].getSimpleName, condition.getClass.getSimpleName, condition.loc)
      },
      ("if then", 2) -> {
        case (Seq(condition: TesslaCore.StreamRef, thenCase: TesslaCore.StreamRef), name, loc) =>
          (Seq(name -> TesslaCore.IfThen(condition, thenCase, loc)), TesslaCore.Stream(name, loc))
        case (Seq(condition: TesslaCore.StreamRef, thenCase: TesslaCore.LiteralValue), name, loc) =>
          val liftedThenCase = mkId(name)
          (
            Seq(
              liftedThenCase -> TesslaCore.Default(TesslaCore.Nil(loc), thenCase, loc),
              name -> TesslaCore.IfThen(condition, TesslaCore.Stream(liftedThenCase, loc), loc)
            ),
            TesslaCore.Stream(name, loc)
          )
        case (Seq(TesslaCore.BoolLiteral(true, _), thenCase: TesslaCore.Arg), _, _) =>
          (Seq(), thenCase)
        case (Seq(TesslaCore.BoolLiteral(false, _), _: TesslaCore.Arg), _, loc) =>
          (Seq(), TesslaCore.Nil(loc))
      }
    )

    lazy val globalEnv: Env = builtins.map {
      case (key, b) => key -> BuiltIn(b)
    } ++ globalDefs.map {
      case (name, d) => (name, d.macroArgs.length) -> Definition(d, globalEnv)
    }

    def translateExpression(expr: Ast.Expr, name: String, env: Env): TranslatedExpression = {
      val errorNode: TranslatedExpression = (Seq(), TesslaCore.Stream("$$$error$$$", UnknownLoc))
      tryWithDefault(errorNode) {
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
              lazy val scope: Env = env ++ definitions.map { definition =>
                val uniqDef = definition.copy(name = definition.name.copy(name = mkId(definition.name.name)))
                (definition.name.name, definition.macroArgs.length) -> Definition(uniqDef, scope)
              }
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
          (name, s)
        case Some(_) =>
          throw TypeError("stream", "constant value", loc)
        case None =>
          if (inStreams.contains(name)) (name, TesslaCore.InputStream(name, loc))
          else throw UndefinedVariable(out.name)
      }
    }

    TesslaCore.Specification(streams, inStreams.toSeq, outs)
  }
}

object AstToCore {
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
}