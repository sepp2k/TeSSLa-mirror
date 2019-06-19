package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.{RuleNode, TerminalNode}
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

class TesslaSyntaxToTessla(spec: Seq[TesslaParser.ParseResult])
    extends TranslationPhase.Translator[Tessla.Specification] with TesslaParser.CanParseConstantString {
  override def translateSpec() = {
    Tessla.Specification(spec.flatMap(res => res.tree.statements.asScala.map(translateStatement(_, res.tokens))))
  }

  trait TesslaVisitor[T] extends TesslaSyntaxBaseVisitor[T] {
    override final def visitChildren(node: RuleNode) = {
      throw InternalError("Undefined visitor method", Location.fromNode(node.asInstanceOf[ParserRuleContext]))
    }
  }

  def translateStatement(stat: TesslaSyntax.StatementContext, tokens: CommonTokenStream): Tessla.Statement = {
    new StatementVisitor(tokens).visit(stat)
  }

  def translateDefinition(definition: TesslaSyntax.DefContext) = {
    val annotations = definition.header.annotations.asScala.map(translateAnnotation)
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

  def translateLiteral(literal: TesslaSyntax.LiteralContext): Tessla.Literal = {
    val lit = if (literal.stringLit != null) {
      Tessla.StringLiteral(getConstantString(literal.stringLit))
    } else if (literal.DECINT != null) {
      Tessla.IntLiteral(BigInt(literal.DECINT.getText))
    } else if (literal.HEXINT != null) {
      require(literal.HEXINT.getText.startsWith("0x"))
      Tessla.IntLiteral(BigInt(literal.HEXINT.getText.substring(2), 16))
    } else if (literal.FLOAT != null) {
      Tessla.FloatLiteral(literal.FLOAT.getText.toDouble)
    } else {
      throw InternalError("Unexpected type of literal", Location.fromNode(literal))
    }
    Tessla.Literal(lit, Location.fromNode(literal))
  }

  def translateAnnotation(annotation: TesslaSyntax.AnnotationContext): Tessla.Annotation = {
    val args = annotation.arguments.asScala.map { arg =>
      if (arg.name != null) {
        Tessla.NamedArgument(mkID(arg.name), translateLiteral(arg.literal))
      } else {
        Tessla.PositionalArgument(translateLiteral(arg.literal))
      }
    }
    Tessla.Annotation(mkID(annotation.ID), args, Location.fromNode(annotation))
  }

  def translateBody(body: TesslaSyntax.BodyContext): Tessla.Body = body match {
    case expBody: TesslaSyntax.ExpressionBodyContext =>
      val exp = translateExpression(expBody.expression)
      if (expBody.defs.isEmpty) {
        Tessla.ExpressionBody(exp)
      } else {
        Tessla.ExpressionBody(Tessla.Block(expBody.defs.asScala.map(translateDefinition), exp, Location.fromNode(body)))
      }
    case builtIn: TesslaSyntax.BuiltInBodyContext =>
      Tessla.BuiltInBody(mkID(builtIn.name), Option(builtIn.expression).map(translateExpression))
  }

  class StatementVisitor(tokens: CommonTokenStream) extends TesslaVisitor[Tessla.Statement] {
    override def visitDefinition(definition: TesslaSyntax.DefinitionContext) = {
      translateDefinition(definition.`def`)
    }

    override def visitAnnotationDefinition(annotationDef: TesslaSyntax.AnnotationDefinitionContext) = {
      val endLoc = Location.fromToken(Option(annotationDef.RPAR).getOrElse(annotationDef.ID))
      val loc = Location.fromToken(annotationDef.DEF).merge(endLoc)
      val params = annotationDef.parameters.asScala.map(translateParameter)
      Tessla.AnnotationDefinition(mkID(annotationDef.ID), params, loc)
    }

    override def visitOut(out: TesslaSyntax.OutContext) = {
      val loc = Option(out.ID).map(Location.fromToken).getOrElse(Location.fromNode(out.expression))
      val id = Option(out.ID).map(mkID).getOrElse {
        Tessla.Identifier(tokens.getText(out.expression), Location.fromNode(out.expression))
      }
      Tessla.Out(translateExpression(out.expression), id, loc)
    }

    override def visitOutAll(outAll: TesslaSyntax.OutAllContext) = {
      Tessla.OutAll(Location.fromNode(outAll))
    }

    override def visitPrint(print: TesslaSyntax.PrintContext) = {
      val loc = Location.fromNode(print.expression)
      Tessla.Print(translateExpression(print.expression), loc)
    }

    override def visitIn(in: TesslaSyntax.InContext) = {
      val annotations = in.annotations.asScala.map(translateAnnotation)
      Tessla.In(mkID(in.ID), translateType(in.`type`), annotations, Location.fromNode(in))
    }

    override def visitTypeDefinition(typeDef: TesslaSyntax.TypeDefinitionContext) = {
      val typeParams = typeDef.typeParameters.asScala.map(mkID)
      val loc = Location.fromToken(typeDef.TYPE).merge(Location.fromNode(typeDef.typeBody))
      val body = typeDef.typeBody match {
        case typ: TesslaSyntax.TypeAliasBodyContext =>
          Tessla.TypeAlias(translateType(typ.`type`))
        case builtIn: TesslaSyntax.BuiltInTypeBodyContext =>
          Tessla.BuiltInType(mkID(builtIn.name))
      }
      Tessla.TypeDefinition(mkID(typeDef.name), typeParams, body, loc)
    }

    override def visitModuleDefinition(module: TesslaSyntax.ModuleDefinitionContext) = {
      val contents = module.contents.asScala.map(visit)
      Tessla.Module(mkID(module.name), contents, Location.fromNode(module))
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
      if (typ.DOLLAR_BRACE != null) {
        warn(Location.fromToken(typ.DOLLAR_BRACE), "Use of '${' for objects is deprecated, use '{' instead")
      }
      val memberTypes = typ.memberSigs.asScala.map { sig =>
        (mkID(sig.name), translateType(sig.`type`))
      }
      Tessla.ObjectType(memberTypes, isOpen = typ.DOTDOT != null, Location.fromNode(typ))
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

  def parseEscapeSequence(sequence: String, loc: Location): String = {
    TesslaParser.parseEscapeSequence(sequence).getOrElse {
      error(InvalidEscapeSequence(sequence, loc))
      sequence
    }
  }

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

    def translateArgument(arg: TesslaSyntax.ArgContext): Tessla.Argument[Tessla.Expression] = {
      if (arg.name != null) {
        Tessla.NamedArgument(mkID(arg.name), translateExpression(arg.expression))
      } else {
        Tessla.PositionalArgument(translateExpression(arg.expression))
      }
    }

    override def visitTupleExpression(exp: TesslaSyntax.TupleExpressionContext) = {
      if (exp.elems.size == 1 && exp.lastComma == null) {
        visit(exp.elems.get(0))
      } else {
        Tessla.Tuple(exp.elems.asScala.map(visit), Location.fromNode(exp))
      }
    }

    override def visitObjectLiteral(obj: TesslaSyntax.ObjectLiteralContext) = {
      if (obj.DOLLAR_BRACE != null) {
        warn(Location.fromToken(obj.DOLLAR_BRACE), "Use of '${' for objects is deprecated, use '{' instead")
      }
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
      if (lambda.funKW != null) {
        warn(Location.fromToken(lambda.funKW), "The keyword 'fun' is deprecated")
      }
      Tessla.Lambda(lambda.params.asScala.map(translateParameter), headerLoc, body, Location.fromNode(lambda))
    }

    def translateOperator(operator: Token, operatorMap: Map[String, String]) = {
      val loc = Location.fromToken(operator)
      val functionName = Tessla.Identifier(operatorMap(operator.getText), loc)
      // TODO: Once the operators are properly located in Predef
      // Tessla.MemberAccess(Tessla.Variable(Tessla.Identifier("Predef", loc)), functionName, loc)
      Tessla.Variable(functionName)
    }

    override def visitUnaryExpression(exp: TesslaSyntax.UnaryExpressionContext) = {
      Tessla.MacroCall(
        translateOperator(exp.op, Tessla.unaryOperators),
        Seq(),
        Seq(Tessla.PositionalArgument(translateExpression(exp.expression))),
        Location.fromNode(exp)
      )
    }

    override def visitInfixExpression(exp: TesslaSyntax.InfixExpressionContext) = {
      Tessla.MacroCall(
        translateOperator(exp.op, Tessla.binaryOperators),
        Seq(),
        Seq(Tessla.PositionalArgument(translateExpression(exp.lhs)), Tessla.PositionalArgument(translateExpression(exp.rhs))),
        Location.fromNode(exp)
      )
    }

    override def visitITE(ite: TesslaSyntax.ITEContext) = {
      val cond = translateExpression(ite.condition)
      val thenCase = translateExpression(ite.thenCase)
      val loc = Location.fromNode(ite)
      val elseCase = translateExpression(ite.elseCase)
      if (ite.STATIC != null) {
        Tessla.StaticIfThenElse(cond, thenCase, elseCase, loc)
      } else {
        Tessla.MacroCall(
          translateOperator(ite.ifToken, Map("if" -> "__ite__")),
          Seq(),
          Seq(Tessla.PositionalArgument(cond), Tessla.PositionalArgument(thenCase), Tessla.PositionalArgument(elseCase)),
          loc
        )
      }
    }

    override def visitMemberAccess(ma: TesslaSyntax.MemberAccessContext) = {
      Tessla.MemberAccess(translateExpression(ma.obj), mkID(ma.fieldName), Location.fromNode(ma))
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
        Tessla.Literal(mkLit(BigInt(intLit.DECINT.getText)), Location.fromNode(intLit))
      } else {
        require(intLit.HEXINT != null)
        require(intLit.HEXINT.getText.startsWith("0x"))
        val x = BigInt(intLit.HEXINT.getText.substring(2), 16)
        Tessla.Literal(mkLit(x), Location.fromNode(intLit))
      }
    }

    override def visitFloatLiteral(floatLit: TesslaSyntax.FloatLiteralContext) = {
      Tessla.Literal(Tessla.FloatLiteral(floatLit.FLOAT.getText.toDouble), Location.fromNode(floatLit))
    }

    override def visitStringLiteral(str: TesslaSyntax.StringLiteralContext): Tessla.Expression = {
      var curStr = new StringBuilder
      var curStrLoc: Option[Location] = Some(Location.fromToken(str.stringLit.openingQuote))
      val parts = new ArrayBuffer[Tessla.Expression]
      str.stringLit.stringContents.forEach {
        case text: TesslaSyntax.TextContext =>
          curStr ++= text.getText
          val loc = Location.fromNode(text)
          curStrLoc = Some(curStrLoc.map(_.merge(loc)).getOrElse(loc))
        case escapeSequence: TesslaSyntax.EscapeSequenceContext =>
          val loc = Location.fromNode(escapeSequence)
          curStr ++= parseEscapeSequence(escapeSequence.getText, loc)
          curStrLoc = Some(curStrLoc.map(_.merge(loc)).getOrElse(loc))
        case format: TesslaSyntax.FormatContext =>
          val formatLoc = Location.fromNode(format)
          TesslaParser.parseFormatString(format.getText, formatLoc) match {
            case invalid: TesslaParser.InvalidFormat =>
              error(invalid.err)
            case noArgs: TesslaParser.NoArgFormat =>
              curStr ++= noArgs.processedString
            case _: TesslaParser.SingleArgFormat =>
              error(FormatNeedsArgument(format.getText, formatLoc))
          }
        case part: TesslaSyntax.StringInterpolationContext =>
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
          if (part.FORMAT == null) {
            parts += Tessla.MacroCall(
              Tessla.Variable(Tessla.Identifier("toString", exp.loc)),
              Seq(), Seq(Tessla.PositionalArgument(exp)), exp.loc
            )
          } else {
            val format = part.FORMAT.getText
            val formatLoc = Location.fromToken(part.FORMAT)
            TesslaParser.parseFormatString(format, formatLoc) match {
              case invalid: TesslaParser.InvalidFormat =>
                error(invalid.err)
              case noArgs: TesslaParser.NoArgFormat =>
                parts += Tessla.MacroCall(
                  Tessla.Variable(Tessla.Identifier("toString", exp.loc)),
                  Seq(), Seq(Tessla.PositionalArgument(exp)), exp.loc
                )
                curStr ++= noArgs.processedString
              case oneArg: TesslaParser.SingleArgFormat =>
                val formatString = Tessla.Literal(Tessla.StringLiteral(format), formatLoc)
                parts += Tessla.MacroCall(
                  Tessla.Variable(Tessla.Identifier(oneArg.formatFunction, exp.loc)),
                  Seq(), Seq(Tessla.PositionalArgument(formatString), Tessla.PositionalArgument(exp)), exp.loc
                )
            }
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

object TesslaSyntaxToTessla extends TranslationPhase[Seq[TesslaParser.ParseResult], Tessla.Specification] {
  override def translate(spec: Seq[TesslaParser.ParseResult]) = {
    new TesslaSyntaxToTessla(spec).translate()
  }
}