package de.uni_luebeck.isp.tessla

import scala.util.{Failure, Success, Try}


object DefExtractor extends CompilerPass[Ast.Spec, Definitions] {
  import scala.language.postfixOps

  case class RedefinitionError(
    name: String,
    first: NestedLoc,
    redef: NestedLoc) extends Fatal

  case class DuplicateArgumentNameError(
    macroName: (String, NestedLoc),
    name: String,
    first: NestedLoc,
    redef: NestedLoc) extends Fatal

  case class MacroArgumentUsedAsFunctionError(
    macroName: (String, NestedLoc),
    name: String,
    defined: NestedLoc,
    used: NestedLoc) extends Fatal

  case class PositionalFollowsNamedArgumentError(
    functionName: (String, NestedLoc),
    name: (String, NestedLoc),
    named: NestedLoc,
    positional: NestedLoc) extends Fatal

  case class DuplicateNamedArgumentError(
    functionName: (String, NestedLoc),
    name: String,
    first: NestedLoc,
    reuse: NestedLoc) extends Fatal

  override def apply(compiler: Compiler, ast: Ast.Spec) = Try {
    var macroDefs = Map[String, MacroDef]()
    var streamDefs = Map[String, StreamDef]()
    var definedNames = Map[String, NestedLoc]()
    var outStreams = Map[String, OutDef]()

    ast.statements.foreach {
      case Ast.Out(name, loc) =>
        outStreams += name.name -> OutDef(name.name, loc)

      case Ast.In(name, typeAscr, loc) =>
        val (type_, _) = buildType(typeAscr)
        streamDefs += name.name -> StreamDef(name.name, loc, ExprTree(InputFn(name.name, type_, loc), Map(), loc))
        if (definedNames contains name.name) {
          compiler diagnostic RedefinitionError(
            name.name, definedNames(name.name), name.loc)
        } else {
          definedNames += name.name -> name.loc
        }

      case Ast.Def(name, args, typeAscr, expr, loc) =>

        val macroDiag = (name.name, name.loc)

        var definedMacroArgNames = Map[String, NestedLoc]()

        val macroArgs = args map {arg =>
          if (definedMacroArgNames contains arg.name.name) {
            compiler diagnostic DuplicateArgumentNameError(
              macroDiag, arg.name.name,
              definedMacroArgNames(arg.name.name), arg.name.loc)
          } else {
            definedMacroArgNames += arg.name.name -> arg.name.loc
          }
          (arg.name.name, arg.name.loc)
        }

        val macroArgSet = macroArgs map (_._1) toSet

        val macroArgTypes = args flatMap {
          x => x.typeAscr map {t => x.name.name -> t}} toMap

        def buildExpr(expr: Ast.Expr): ExprTree = {
          expr match {
            case Ast.ExprName(cname) =>
              buildOptTypeAscr(
                macroArgTypes get cname.name,
                ExprTree(NamedFn(cname.name, cname.loc), Map(), cname.loc),
                mergeLocs = false)
            case Ast.ExprApp(fname, fargs, floc) =>
              if (macroArgSet contains fname.name) {
                compiler diagnostic MacroArgumentUsedAsFunctionError(
                  macroDiag, fname.name, definedMacroArgNames(fname.name), fname.loc)
              }
              var usedArgNames = Map[String, NestedLoc]()
              var lastNamedLoc: Option[((String, NestedLoc), NestedLoc)] = None
              val treeArgs: Map[ArgName, ExprTree] = fargs.zipWithIndex map {
                case (Ast.PosArg(aexpr), idx) =>
                  val res = buildExpr(aexpr)
                  if (lastNamedLoc.nonEmpty) {
                    compiler diagnostic PositionalFollowsNamedArgumentError(
                      (fname.name, fname.loc),
                      lastNamedLoc.get._1,
                      lastNamedLoc.get._2,
                      res.loc)
                  }
                  Pos(idx) -> res
                case (Ast.NamedArg(aname, aexpr), _) =>
                  val res = buildExpr(aexpr)
                  if (usedArgNames contains aname.name) {
                    compiler diagnostic DuplicateNamedArgumentError(
                      (fname.name, fname.loc),
                      aname.name,
                      usedArgNames(aname.name),
                      aname.loc)
                  } else {
                    usedArgNames += aname.name -> aname.loc
                  }
                  lastNamedLoc = Some(((aname.name, aname.loc), res.loc))
                  // TODO don't lose aname.loc
                  Named(aname.name) -> res
              } toMap

              ExprTree(NamedFn(fname.name, fname.loc), treeArgs, loc)
            case Ast.ExprGrouped(gexpr, gloc) =>
              buildExpr(gexpr).copy(loc = gloc)
            case Ast.ExprTypeAscr(texpr, ty) =>
              buildTypeAscr(ty, buildExpr(texpr), mergeLocs = true)
            case Ast.ExprIntLit(value) =>
              val fn = LiteralFn(IntLiteral(value.value), value.loc)
              ExprTree(fn, Map(), value.loc)
            case Ast.ExprStringLit(value) =>
              val fn = LiteralFn(StringLiteral(value.value), value.loc)
              ExprTree(fn, Map(), value.loc)
            case Ast.ExprBoolLit(value) =>
              val fn = LiteralFn(BoolLiteral(value.value), value.loc)
              ExprTree(fn, Map(), value.loc)
            case Ast.ExprFloatLit(value) =>
              val fn = LiteralFn(FloatLiteral(value.value), value.loc)
              ExprTree(fn, Map(), value.loc)
          }
        }

        val tree = buildOptTypeAscr(typeAscr, buildExpr(expr), mergeLocs = false)
        val streamDef = StreamDef(name.name, loc, tree)
        if (args.isEmpty) {
          streamDefs += name.name -> streamDef
        } else {
          val macroDef = MacroDef(macroArgs, streamDef)
          macroDefs += name.name -> macroDef
        }
        if (definedNames contains name.name) {
          compiler diagnostic RedefinitionError(
            name.name, definedNames(name.name), name.loc)
        } else {
          definedNames += name.name -> name.loc
        }
    }

    def buildOptTypeAscr(
      typeAscr: Option[Ast.Type], expr: ExprTree, mergeLocs: Boolean
    ): ExprTree = {
      typeAscr match {
        case None => expr
        case Some(ty) => buildTypeAscr(ty, expr, mergeLocs)
      }
    }

    def buildTypeAscr(
      typeAscr: Ast.Type, expr: ExprTree, mergeLocs: Boolean
    ): ExprTree = {
      val (ty, loc) = buildType(typeAscr)
      val exprLoc = if (mergeLocs) expr.loc.merge(loc) else expr.loc
      ExprTree(TypeAscrFn(ty, loc), Map(Pos(0) -> expr), exprLoc)
    }

    def buildType(ty: Ast.Type): (Type, NestedLoc) = ty match {
      case Ast.TypeName(name) => (SimpleType(name.name), name.loc)
      case Ast.TypeApp(name, args, loc) =>
        (GenericType(name.name, args map {x => buildType(x)._1}), loc)
    }

    Definitions(streamDefs, macroDefs, outStreams)
  }
}
