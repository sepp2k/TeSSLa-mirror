package de.uni_luebeck.isp.tessla

import java.nio.file.Paths

import de.uni_luebeck.isp.tessla.Errors._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.{RuleNode, TerminalNode}

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer


class TesslaParser extends TranslationPhase[CharStream, Tessla.Specification] {
  var path: String = _

  override def translateSpec(input: CharStream) = {
    path = input.getSourceName
    val tokens = new CommonTokenStream(new TesslaLexer(input))
    val parser = new TesslaSyntax(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new BaseErrorListener {
      override def syntaxError(r: Recognizer[_, _], offendingToken: Any, l: Int, c: Int, msg: String, e: RecognitionException) = {
        error(Errors.ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token])))
      }
    })
    val spec = parser.spec()
    if (errors.nonEmpty) {
      val lastError = errors.remove(errors.length - 1)
      throw lastError
    }
    Tessla.Specification(spec.includes.asScala.flatMap(translateInclude) ++
      spec.statements.asScala.map(translateStatement))
  }

  def translateInclude(include: TesslaSyntax.IncludeContext): Seq[Tessla.Statement] = {
    // getParent returns null for relative paths without subdirectories (i.e. just a file name), which is
    // annoying and stupid. So we wrap the call in an option and fall back to "." as the default.
    val dir = Option(Paths.get(path).getParent).getOrElse(Paths.get("."))
    val includePath = dir.resolve(getIncludeString(include.file))
    translateSpec(CharStreams.fromFileName(includePath.toString)).statements
  }

  def parseEscapeSequence(sequence: String, loc: Location): String = sequence match {
    case "\\r" => "\r"
    case "\\n" => "\n"
    case "\\t" => "\t"
    case "\\a" => "\u0007"
    case "\\\\" => "\\"
    case "\\\"" => "\""
    case other =>
      error(InvalidEscapeSequence(other, loc))
      other
  }

  def getIncludeString(stringLit: TesslaSyntax.StringLitContext): String = {
    stringLit.stringContents.asScala.map { part =>
      if (part.TEXT != null) part.TEXT.getText
      else if (part.ESCAPE_SEQUENCE != null) {
        parseEscapeSequence(part.ESCAPE_SEQUENCE.getText, Location.fromNode(part))
      } else {
        error(StringInterpolationInInclude(Location.fromNode(part)))
        ""
      }
    }.mkString
  }

  trait TesslaVisitor[T] extends TesslaSyntaxBaseVisitor[T] {
    override final def visitChildren(node: RuleNode) = {
      throw InternalError("Undefined visitor method")
    }
  }

  def translateStatement(stat: TesslaSyntax.StatementContext): Tessla.Statement = StatementVisitor.visit(stat)

  def translateDefinition(definition: TesslaSyntax.DefContext) = {
    val annotations = definition.header.annotations.asScala.map(_.ID).map(mkID)
    val typeParameters = definition.header.typeParameters.asScala.map(mkID)
    val parameters = definition.header.parameters.asScala.map(translateParameter)
    val body = translateBody(definition.body)
    val loc = Location.fromToken(definition.header.DEF).merge(Location.fromNode(definition.body))
    Tessla.Definition(
      annotations, mkID(definition.header.name), typeParameters, parameters,
      Option(definition.header.resultType).map(translateType),
      Location.fromNode(definition.header), body, loc
    )
  }

  def translateBody(body: TesslaSyntax.BodyContext): Tessla.Expression = {
    val exp = translateExpression(body.expression)
    if (body.defs.isEmpty) {
      exp
    } else {
      Tessla.Block(body.defs.asScala.map(translateDefinition), exp, Location.fromNode(body))
    }
  }

  object StatementVisitor extends TesslaVisitor[Tessla.Statement] {
    override def visitDefinition(definition: TesslaSyntax.DefinitionContext) = {
      translateDefinition(definition.`def`)
    }

    override def visitOut(out: TesslaSyntax.OutContext) = {
      val loc = Option(out.ID).map(Location.fromToken).getOrElse(Location.fromNode(out.expression))
      Tessla.Out(translateExpression(out.expression), Option(out.ID).map(mkID), loc)
    }

    override def visitOutAll(outAll: TesslaSyntax.OutAllContext) = {
      Tessla.OutAll(Location.fromNode(outAll))
    }

    override def visitPrint(print: TesslaSyntax.PrintContext) = {
      val loc = Location.fromNode(print.expression)
      Tessla.Print(translateExpression(print.expression), loc)
    }

    override def visitIn(in: TesslaSyntax.InContext) = {
      Tessla.In(mkID(in.ID), translateType(in.`type`), Location.fromNode(in))
    }

    override def visitTypeDefinition(typeDef: TesslaSyntax.TypeDefinitionContext) = {
      val typeParams = typeDef.typeParameters.asScala.map(mkID)
      val loc = Location.fromToken(typeDef.TYPE).merge(Location.fromNode(typeDef.`type`))
      Tessla.TypeDefinition(mkID(typeDef.name), typeParams, translateType(typeDef.`type`), loc)
    }
  }

  def translateParameter(parameter: TesslaSyntax.ParamContext): Tessla.Parameter = {
    Tessla.Parameter(mkID(parameter.ID), Option(parameter.parameterType).map(translateType))
  }

  def translateType(typ: TesslaSyntax.TypeContext): Tessla.Type = TypeVisitor.visit(typ)

  object TypeVisitor extends TesslaVisitor[Tessla.Type] {
    override def visitSimpleType(typ: TesslaSyntax.SimpleTypeContext) = {
      Tessla.SimpleType(mkID(typ.ID))
    }

    override def visitTypeApplication(typ: TesslaSyntax.TypeApplicationContext) = {
      val loc = Location.fromNode(typ)
      Tessla.TypeApplication(mkID(typ.ID), typ.typeArguments.asScala.map(translateType), loc)
    }

    override def visitFunctionType(typ: TesslaSyntax.FunctionTypeContext) = {
      val loc = Location.fromNode(typ)
      Tessla.FunctionType(typ.parameterTypes.asScala.map(translateType), translateType(typ.resultType), loc)
    }

    override def visitObjectType(typ: TesslaSyntax.ObjectTypeContext) = {
      val memberTypes = typ.memberSigs.asScala.map { sig =>
        (mkID(sig.name), translateType(sig.`type`))
      }
      Tessla.ObjectType(memberTypes, Location.fromNode(typ))
    }

    override def visitTupleType(typ: TesslaSyntax.TupleTypeContext) = {
      Tessla.TupleType(typ.elementTypes.asScala.map(translateType), Location.fromNode(typ))
    }
  }

  def mkID(id: Token): Tessla.Identifier = {
    Tessla.Identifier(id.getText, Location.fromToken(id))
  }

  def mkID(id: TerminalNode): Tessla.Identifier = {
    mkID(id.getSymbol)
  }

  def translateExpression(exp: TesslaSyntax.ExpressionContext): Tessla.Expression = ExpressionVisitor.visit(exp)

  object ExpressionVisitor extends TesslaVisitor[Tessla.Expression] {
    override def visitVariable(variable: TesslaSyntax.VariableContext): Tessla.Variable = {
      Tessla.Variable(mkID(variable.ID))
    }

    override def visitFunctionCall(funCall: TesslaSyntax.FunctionCallContext) = {
      val typeArgs = funCall.typeArguments.asScala.map(translateType)
      val args = funCall.arguments.asScala.map(translateArgument)
      val loc = Location.fromNode(funCall)
      Tessla.MacroCall(translateExpression(funCall.function), typeArgs, args, loc)
    }

    def translateArgument(arg: TesslaSyntax.ArgContext): Tessla.Argument = {
      if (arg.name != null) {
        Tessla.NamedArgument(mkID(arg.name), translateExpression(arg.expression))
      } else {
        Tessla.PositionalArgument(translateExpression(arg.expression))
      }
    }

    override def visitTupleExpression(exp: TesslaSyntax.TupleExpressionContext) = {
      if (exp.elems.isEmpty) {
        Tessla.Literal(Tessla.Unit, Location.fromNode(exp))
      } else if (exp.elems.size == 1 && exp.lastComma == null) {
        visit(exp.elems.get(0))
      } else {
        Tessla.Tuple(exp.elems.asScala.map(visit), Location.fromNode(exp))
      }
    }

    override def visitObjectLiteral(obj: TesslaSyntax.ObjectLiteralContext) = {
      Tessla.ObjectLiteral(obj.members.asScala.map(translateMemberDefinition), Location.fromNode(obj))
    }

    def translateMemberDefinition(memDef: TesslaSyntax.MemberDefinitionContext) = {
      if (memDef.value == null) Tessla.MemberDefinition.Simple(mkID(memDef.name))
      else Tessla.MemberDefinition.Full(mkID(memDef.name), translateExpression(memDef.value))
    }

    override def visitBlock(block: TesslaSyntax.BlockContext) = {
      val defs = block.definitions.asScala.map(translateDefinition)
      if (block.RETURN != null) {
        warn(Location.fromToken(block.RETURN), "The keyword 'return' is deprecated")
      }
      Tessla.Block(defs, translateExpression(block.expression), Location.fromNode(block))
    }

    override def visitLambda(lambda: TesslaSyntax.LambdaContext) = {
      val startHeaderLoc = Location.fromToken(Option(lambda.funKW).getOrElse(lambda.openingParen))
      val endHeaderLoc = Location.fromToken(lambda.closingParen)
      val headerLoc = startHeaderLoc.merge(endHeaderLoc)
      val body = translateExpression(lambda.expression)
      Tessla.Lambda(lambda.params.asScala.map(translateParameter), headerLoc, body, Location.fromNode(lambda))
    }

    override def visitUnaryExpression(exp: TesslaSyntax.UnaryExpressionContext) = {
      val op = if (exp.op.getText == "-") "unary -" else exp.op.getText
      Tessla.MacroCall(
        Tessla.Variable(Tessla.Identifier(op, Location.fromToken(exp.op))),
        Seq(),
        Seq(Tessla.PositionalArgument(translateExpression(exp.expression))),
        Location.fromNode(exp)
      )
    }

    override def visitInfixExpression(exp: TesslaSyntax.InfixExpressionContext) = {
      Tessla.MacroCall(
        Tessla.Variable(mkID(exp.op)),
        Seq(),
        Seq(Tessla.PositionalArgument(translateExpression(exp.lhs)), Tessla.PositionalArgument(translateExpression(exp.rhs))),
        Location.fromNode(exp)
      )
    }

    override def visitITE(ite: TesslaSyntax.ITEContext) = {
      val cond = translateExpression(ite.condition)
      val thenCase = translateExpression(ite.thenCase)
      val loc = Location.fromNode(ite)
      if (ite.elseCase == null) {
        Tessla.MacroCall(
          Tessla.Variable(Tessla.Identifier("if then", Location.fromToken(ite.ifToken))),
          Seq(),
          Seq(Tessla.PositionalArgument(cond), Tessla.PositionalArgument(thenCase)), loc
        )
      } else {
        val elseCase = translateExpression(ite.elseCase)
        if (ite.STATIC != null) {
          Tessla.StaticIfThenElse(cond, thenCase, elseCase, loc)
        } else {
          Tessla.MacroCall(
            Tessla.Variable(Tessla.Identifier("if then else", Location.fromToken(ite.ifToken))),
            Seq(),
            Seq(Tessla.PositionalArgument(cond), Tessla.PositionalArgument(thenCase), Tessla.PositionalArgument(elseCase)),
            loc
          )
        }
      }
    }

    override def visitMemberAccess(ma: TesslaSyntax.MemberAccessContext) = {
      Tessla.MemberAccess(translateExpression(ma.obj), mkID(ma.fieldName), Location.fromNode(ma))
    }

    override def visitTrue(trueExp: TesslaSyntax.TrueContext) = {
      Tessla.Literal(Tessla.BoolLiteral(true), Location.fromNode(trueExp))
    }

    override def visitFalse(falseExp: TesslaSyntax.FalseContext) = {
      Tessla.Literal(Tessla.BoolLiteral(false), Location.fromNode(falseExp))
    }

    override def visitIntLiteral(intLit: TesslaSyntax.IntLiteralContext) = {
      def mkLit(x: BigInt) = {
        if (intLit.timeUnit == null) {
          Tessla.IntLiteral(x)
        } else {
          Tessla.TimeLiteral(x, TimeUnit.fromString(intLit.timeUnit.getText, Location.fromToken(intLit.timeUnit)))
        }
      }
      if (intLit.DECINT != null) {
        Tessla.Literal(mkLit(intLit.DECINT.getText.toInt), Location.fromNode(intLit))
      } else {
        require(intLit.HEXINT != null)
        require(intLit.HEXINT.getText.startsWith("0x"))
        val x = Integer.parseInt(intLit.HEXINT.getText.substring(2), 16)
        Tessla.Literal(mkLit(x), Location.fromNode(intLit))
      }
    }

    override def visitStringLiteral(str: TesslaSyntax.StringLiteralContext): Tessla.Expression = {
      var curStr = new StringBuilder
      var curStrLoc: Option[Location] = Some(Location.fromToken(str.stringLit.openingQuote))
      val parts = new ArrayBuffer[Tessla.Expression]
      str.stringLit.stringContents.forEach {part =>
        val partLoc = Location.fromNode(part)
        if (part.TEXT != null) {
          curStr ++= part.TEXT.getText
          curStrLoc = Some(curStrLoc.map(_.merge(partLoc)).getOrElse(partLoc))
        } else if (part.ESCAPE_SEQUENCE != null) {
          curStr ++= parseEscapeSequence(part.ESCAPE_SEQUENCE.getText, partLoc)
          curStrLoc = Some(curStrLoc.map(_.merge(partLoc)).getOrElse(partLoc))
        } else {
          if (curStr.nonEmpty) {
            parts += Tessla.Literal(Tessla.StringLiteral(curStr.toString), curStrLoc.get)
            curStrLoc = None
            curStr = new StringBuilder
          }
          val exp = if (part.ID != null) {
            Tessla.Variable(mkID(part.ID))
          } else {
            translateExpression(part.expression)
          }
          parts += Tessla.MacroCall(
            Tessla.Variable(Tessla.Identifier("toString", exp.loc)),
            Seq(), Seq(Tessla.PositionalArgument(exp)), exp.loc
          )
        }
      }
      if (curStr.nonEmpty) {
        val loc = curStrLoc.get.merge(Location.fromToken(str.stringLit.closingQuote))
        parts += Tessla.Literal(Tessla.StringLiteral(curStr.toString), loc)
      }
      if (parts.isEmpty) {
        Tessla.Literal(Tessla.StringLiteral(""), Location.fromNode(str))
      } else {
        parts.reduceLeft { (acc, exp) =>
          Tessla.MacroCall(
            Tessla.Variable(Tessla.Identifier("String_concat", exp.loc)),
            Seq(),
            Seq(Tessla.PositionalArgument(acc), Tessla.PositionalArgument(exp)),
            exp.loc
          )
        }
      }
    }
  }
}
