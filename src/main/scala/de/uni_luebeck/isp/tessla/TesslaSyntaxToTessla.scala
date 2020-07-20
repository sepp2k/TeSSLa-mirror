package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.pattern.RuleTagToken
import org.antlr.v4.runtime.tree.{RuleNode, TerminalNode}

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuffer

import cats.implicits._

class TesslaSyntaxToTessla(spec: Seq[TesslaParser.ParseResult])
    extends TranslationPhase.Translator[Tessla.Specification]
    with TesslaParser.CanParseConstantString {
  override def translateSpec() = {
    val statements =
      spec.flatMap(res => res.tree.entries.asScala.map(_.statement).map(translateStatement(_, res.tokens)))
    checkForDuplicates(statements.flatMap(Tessla.getId))
    checkForDuplicates(statements.flatMap(getTypeDefID))
    Tessla.Specification(statements)
  }

  def getTypeDefID(stat: Tessla.Statement) = stat match {
    case typeDef: Tessla.TypeDefinition => Some(typeDef.id)
    case _                              => None
  }

  trait TesslaVisitor[T] extends TesslaSyntaxBaseVisitor[T] {
    final override def visitChildren(node: RuleNode) = {
      throw InternalError(
        "Undefined visitor method",
        Location.fromNode(node.asInstanceOf[ParserRuleContext])
      )
    }
  }

  def translateStatement(
    stat: TesslaSyntax.StatementContext,
    tokens: CommonTokenStream
  ): Tessla.Statement = {
    new StatementVisitor(tokens).visit(stat)
  }

  def translateDefinition(definition: TesslaSyntax.DefContext): Tessla.Definition = {
    val liftable = definition.header.liftable != null
    val typeParameters = definition.header.typeParameters.asScala.map(mkID)
    checkForDuplicates(typeParameters.toSeq)
    val parameters = definition.header.parameters.asScala.map(translateParameter)
    val paramIDs = parameters.map(_._2.id)
    checkForDuplicates(paramIDs.toSeq)
    val body = translateBody(definition.body)
    body match {
      case Tessla.ExpressionBody(block: Tessla.Block) =>
        block.definitions.foreach { definition =>
          paramIDs.find(_ == definition.id).foreach { paramID =>
            error(MultipleDefinitionsError(definition.id, paramID.loc))
          }
        }
      case _ =>
    }
    val loc = Location.fromToken(definition.header.DEF).merge(Location.fromNode(definition.body))
    Tessla.Definition(
      mkID(definition.header.name),
      typeParameters.toSeq,
      parameters.toSeq,
      Option(definition.header.resultType).map(translateType),
      Location.fromNode(definition.header),
      body,
      loc,
      liftable
    )
  }

  def checkForDuplicates(identifiers: Seq[Tessla.Identifier]): Unit = {
    identifiers.groupBy(_.name).foreach {
      case (_, duplicates) if duplicates.lengthCompare(1) > 0 =>
        val firstLoc = duplicates.head.loc
        duplicates.tail.foreach { duplicate =>
          error(MultipleDefinitionsError(duplicate, firstLoc))
        }

      case _ => /* Do nothing */
    }
  }

  def translateConstantExpression(
    exp: TesslaSyntax.ConstantExpressionContext
  ): Tessla.ConstantExpression = exp match {
    case lit: TesslaSyntax.ConstantLiteralContext =>
      val translatedLit = if (lit.stringLit != null) {
        Tessla.StringLiteral(getConstantString(lit.stringLit))
      } else if (lit.DECINT != null) {
        Tessla.IntLiteral(BigInt(lit.DECINT.getText))
      } else if (lit.HEXINT != null) {
        require(lit.HEXINT.getText.startsWith("0x"))
        Tessla.IntLiteral(BigInt(lit.HEXINT.getText.substring(2), 16))
      } else if (lit.FLOAT != null) {
        Tessla.FloatLiteral(lit.FLOAT.getText.toDouble)
      } else {
        throw InternalError("Unexpected type of literal", Location.fromNode(lit))
      }
      Tessla.ConstantExpression.Literal(translatedLit, Location.fromNode(lit))
    case obj: TesslaSyntax.ConstantObjectContext =>
      val members = obj.members.asScala.map { member =>
        mkID(member.name) -> translateConstantExpression(member.value)
      }
      checkForDuplicates(members.map(_._1).toSeq)
      Tessla.ConstantExpression.Object(members.toMap, Location.fromNode(obj))
    case tup: TesslaSyntax.ConstantTupleContext =>
      val elements = tup.elems.asScala.map(translateConstantExpression)
      Tessla.ConstantExpression.Object(tupleToObject(elements.toSeq), Location.fromNode(tup))
  }

  def translateAnnotation(annotation: TesslaSyntax.AnnotationContext): Tessla.Annotation = {
    val args = annotation.arguments.asScala.map { arg =>
      if (arg.name != null) {
        Tessla.NamedArgument(mkID(arg.name), translateConstantExpression(arg.constantExpression))
      } else {
        Tessla.PositionalArgument(translateConstantExpression(arg.constantExpression))
      }
    }
    Tessla.Annotation(mkID(annotation.ID), args.toSeq, Location.fromNode(annotation))
  }

  def translateBody(body: TesslaSyntax.BodyContext): Tessla.Body = body match {
    case expBody: TesslaSyntax.ExpressionBodyContext =>
      val exp = translateExpression(expBody.expression)
      if (expBody.defs.isEmpty) {
        Tessla.ExpressionBody(exp)
      } else {
        Tessla.ExpressionBody(
          Tessla.Block(
            expBody.defs.asScala.map(translateDefinition).toSeq,
            exp,
            Location.fromNode(body)
          )
        )
      }
    case builtIn: TesslaSyntax.BuiltInBodyContext =>
      Tessla.Extern(mkID(builtIn.name.content), Option(builtIn.expression).map(translateExpression))
  }

  class StatementVisitor(tokens: CommonTokenStream) extends TesslaVisitor[Tessla.Statement] {
    override def visitDefinition(definition: TesslaSyntax.DefinitionContext) = {
      translateDefinition(definition.`def`)
    }

    override def visitAnnotationDefinition(
      annotationDef: TesslaSyntax.AnnotationDefinitionContext
    ) = {
      val endLoc = Location.fromToken(Option(annotationDef.RPAR).getOrElse(annotationDef.ID))
      val loc = Location.fromToken(annotationDef.DEF).merge(endLoc)
      val params = annotationDef.parameters.asScala.map(translateParameter)
      Tessla.AnnotationDefinition(mkID(annotationDef.ID), params.toSeq.map(_._2), loc)
    }

    override def visitOut(out: TesslaSyntax.OutContext) = {
      val annotations = out.annotations.asScala.map(translateAnnotation)
      if (out.star == null) {
        val loc =
          Option(out.ID).map(Location.fromToken).getOrElse(Location.fromNode(out.expression))
        val id = Option(out.ID).map(mkID).getOrElse {
          Tessla.Identifier(tokens.getText(out.expression), Location.fromNode(out.expression))
        }
        Tessla.Out(translateExpression(out.expression), id, annotations.toSeq, loc)
      } else {
        Tessla.OutAll(annotations.toSeq, Location.fromNode(out))
      }
    }

    override def visitIn(in: TesslaSyntax.InContext) = {
      val annotations = in.annotations.asScala.map(translateAnnotation)
      Tessla.In(mkID(in.ID), translateType(in.`type`), annotations.toSeq, Location.fromNode(in))
    }

    override def visitTypeDefinition(typeDef: TesslaSyntax.TypeDefinitionContext) = {
      val typeParams = typeDef.typeParameters.asScala.map(mkID)
      val loc = Location.fromToken(typeDef.TYPE).merge(Location.fromNode(typeDef.typeBody))
      val body = typeDef.typeBody match {
        case typ: TesslaSyntax.TypeAliasBodyContext =>
          Tessla.TypeAlias(translateType(typ.`type`))
        case builtIn: TesslaSyntax.BuiltInTypeBodyContext =>
          Tessla.ExternType(mkID(builtIn.name.content))
      }
      checkForDuplicates(typeParams.toSeq)
      Tessla.TypeDefinition(mkID(typeDef.name), typeParams.toSeq, body, loc)
    }

    override def visitModuleDefinition(module: TesslaSyntax.ModuleDefinitionContext) = {
      val contents = module.contents.asScala.map(_.statement).map(visit)
      Tessla.Module(mkID(module.name), contents.toSeq, Location.fromNode(module))
    }

    override def visitImportStatement(imprt: TesslaSyntax.ImportStatementContext): Tessla.Statement = {
      Tessla.Import(imprt.path.asScala.map(mkID).toList, Location.fromNode(imprt))
    }
  }

  def translateParameter(
    parameter: TesslaSyntax.ParamContext
  ): (Option[TesslaAST.RuntimeEvaluation], Tessla.Parameter) = {
    val (eval, typ) = Option(parameter.parameterType).map(translateEvalType).separate
    (eval.flatten, Tessla.Parameter(mkID(parameter.ID), typ))
  }

  def translateType(typ: TesslaSyntax.TypeContext): Tessla.Type = TypeVisitor.visit(typ)

  def translateEvalType(evTyp: TesslaSyntax.EvalTypeContext): (Option[TesslaAST.RuntimeEvaluation], Tessla.Type) = {
    val typ = TypeVisitor.visit(evTyp.typ)
    val evaluation = Option(evTyp.evaluation).map(e =>
      List(
        TesslaAST.StrictEvaluation,
        TesslaAST.LazyEvaluation
      ).find(_.toString == e.getText).get
    )
    (evaluation, typ)
  }

  def tupleToObject[T <: Location.HasLoc](elems: Seq[T]): Map[Tessla.Identifier, T] = {
    elems.zipWithIndex.map {
      case (elem, index) =>
        Tessla.Identifier(s"_${index + 1}", elem.loc) -> elem
    }.toMap
  }

  object TypeVisitor extends TesslaVisitor[Tessla.Type] {
    override def visitSimpleType(typ: TesslaSyntax.SimpleTypeContext) = {
      Tessla.SimpleType(mkID(typ.ID))
    }

    override def visitTypeApplication(typ: TesslaSyntax.TypeApplicationContext) = {
      val loc = Location.fromNode(typ)
      Tessla.TypeApplication(mkID(typ.ID), typ.typeArguments.asScala.map(translateType).toSeq, loc)
    }

    override def visitFunctionType(typ: TesslaSyntax.FunctionTypeContext) = {
      val loc = Location.fromNode(typ)
      Tessla.FunctionType(
        typ.parameterTypes.asScala.map(translateEvalType).toSeq,
        translateType(typ.resultType),
        loc
      )
    }

    override def visitObjectType(typ: TesslaSyntax.ObjectTypeContext) = {
      if (typ.DOLLAR_BRACE != null) {
        warn(
          Location.fromToken(typ.DOLLAR_BRACE),
          "Use of '${' for objects is deprecated, use '{' instead"
        )
      }
      val memberTypes = typ.memberSigs.asScala.map { sig =>
        (mkID(sig.name), translateType(sig.`type`))
      }
      checkForDuplicates(memberTypes.map(_._1).toSeq)
      Tessla.ObjectType(memberTypes.toMap, isOpen = typ.DOTDOT != null, Location.fromNode(typ))
    }

    override def visitTupleType(typ: TesslaSyntax.TupleTypeContext) = {
      val types = typ.elementTypes.asScala.map(translateType)
      Tessla.ObjectType(tupleToObject(types.toSeq), isOpen = false, Location.fromNode(typ))
    }
  }

  def mkID(id: Token): Tessla.Identifier = {
    Tessla.Identifier(id.getText, Location.fromToken(id))
  }

  def mkID(id: TerminalNode): Tessla.Identifier = {
    mkID(id.getSymbol)
  }

  def translateExpression(exp: TesslaSyntax.ExpressionContext): Tessla.Expression =
    ExpressionVisitor.visit(exp)

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
      Tessla.MacroCall(translateExpression(funCall.function), typeArgs.toSeq, args.toSeq, loc)
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
        val members = tupleToObject(exp.elems.asScala.map(visit).toSeq)
        Tessla.ObjectLiteral(members, Location.fromNode(exp))
      }
    }

    override def visitObjectLiteral(obj: TesslaSyntax.ObjectLiteralContext) = {
      if (obj.DOLLAR_BRACE != null) {
        warn(
          Location.fromToken(obj.DOLLAR_BRACE),
          "Use of '${' for objects is deprecated, use '{' instead"
        )
      }
      val members = obj.members.asScala.map(translateMemberDefinition)
      checkForDuplicates(members.map(_._1).toSeq)
      Tessla.ObjectLiteral(members.toMap, Location.fromNode(obj))
    }

    def translateMemberDefinition(memDef: TesslaSyntax.MemberDefinitionContext) = {
      val id = mkID(memDef.name)
      if (memDef.value == null) id -> Tessla.Variable(id)
      else id -> translateExpression(memDef.value)
    }

    override def visitBlock(block: TesslaSyntax.BlockContext) = {
      val defs = block.definitions.asScala.map(translateDefinition)
      checkForDuplicates(defs.map(_.id).toSeq)
      if (block.RETURN != null) {
        warn(Location.fromToken(block.RETURN), "The keyword 'return' is deprecated")
      }
      Tessla.Block(defs.toSeq, translateExpression(block.expression), Location.fromNode(block))
    }

    override def visitLambda(lambda: TesslaSyntax.LambdaContext) = {
      val startHeaderLoc = Location.fromToken(lambda.openingParen)
      val endHeaderLoc = Location.fromToken(lambda.closingParen)
      val headerLoc = startHeaderLoc.merge(endHeaderLoc)
      val body = translateExpression(lambda.expression)
      val params = lambda.params.asScala.map(translateParameter)
      checkForDuplicates(params.map(_._2.id).toSeq)
      Tessla.Lambda(params.toSeq, headerLoc, body, Location.fromNode(lambda))
    }

    def translateOperator(operator: Token, operatorMap: Map[String, String]) = {
      val loc = Location.fromToken(operator)
      val functionName = Tessla.Identifier(operatorMap(operator.getText), loc)
      Tessla.MemberAccess(
        Tessla.RootMemberAccess(Tessla.Identifier("Operators", loc), loc),
        functionName,
        loc
      )
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
        Seq(
          Tessla.PositionalArgument(translateExpression(exp.lhs)),
          Tessla.PositionalArgument(translateExpression(exp.rhs))
        ),
        Location.fromNode(exp)
      )
    }

    override def visitITE(ite: TesslaSyntax.ITEContext) = {
      val cond = translateExpression(ite.condition)
      val thenCase = translateExpression(ite.thenCase)
      val loc = Location.fromNode(ite)
      val elseCase = translateExpression(ite.elseCase)
      val extern = if (ite.STATIC != null) "staticite" else "ite"
      val op = translateOperator(ite.IF().getSymbol, Map("if" -> extern))
      val args = Seq(
        Tessla.PositionalArgument(cond),
        Tessla.PositionalArgument(thenCase),
        Tessla.PositionalArgument(elseCase)
      )
      Tessla.MacroCall(op, Seq(), args, loc)
    }

    override def visitRootMemberAccess(root: TesslaSyntax.RootMemberAccessContext) = {
      Tessla.RootMemberAccess(mkID(root.fieldName), Location.fromNode(root))
    }

    override def visitMemberAccess(ma: TesslaSyntax.MemberAccessContext) = {
      Tessla.MemberAccess(translateExpression(ma.obj), mkID(ma.fieldName), Location.fromNode(ma))
    }

    override def visitIntLiteral(intLit: TesslaSyntax.IntLiteralContext) = {
      def mkLit(x: BigInt) = {
        if (intLit.timeUnit == null) {
          Tessla.IntLiteral(x)
        } else {
          Tessla.TimeLiteral(
            x,
            TimeUnit.fromString(intLit.timeUnit.getText, Location.fromToken(intLit.timeUnit))
          )
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
      Tessla.Literal(
        Tessla.FloatLiteral(floatLit.FLOAT.getText.toDouble),
        Location.fromNode(floatLit)
      )
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
              Tessla.RootMemberAccess(Tessla.Identifier("toString", exp.loc), exp.loc),
              Seq(),
              Seq(Tessla.PositionalArgument(exp)),
              exp.loc
            )
          } else {
            val format = part.FORMAT.getText
            val formatLoc = Location.fromToken(part.FORMAT)
            TesslaParser.parseFormatString(format, formatLoc) match {
              case invalid: TesslaParser.InvalidFormat =>
                error(invalid.err)
              case noArgs: TesslaParser.NoArgFormat =>
                parts += Tessla.MacroCall(
                  Tessla.RootMemberAccess(Tessla.Identifier("toString", exp.loc), exp.loc),
                  Seq(),
                  Seq(Tessla.PositionalArgument(exp)),
                  exp.loc
                )
                curStr ++= noArgs.processedString
              case oneArg: TesslaParser.SingleArgFormat =>
                val formatString = Tessla.Literal(Tessla.StringLiteral(format), formatLoc)
                parts += Tessla.MacroCall(
                  Tessla.MemberAccess(
                    Tessla.RootMemberAccess(Tessla.Identifier("String", exp.loc), exp.loc),
                    Tessla.Identifier(oneArg.formatFunction, exp.loc),
                    exp.loc
                  ),
                  Seq(),
                  Seq(Tessla.PositionalArgument(formatString), Tessla.PositionalArgument(exp)),
                  exp.loc
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
            Tessla.MemberAccess(
              Tessla.RootMemberAccess(Tessla.Identifier("String", exp.loc), exp.loc),
              Tessla.Identifier("concat", exp.loc),
              exp.loc
            ),
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
