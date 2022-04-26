/*
 * Copyright 2022 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.Errors._
import org.antlr.v4.runtime.tree.{RuleNode, TerminalNode}
import org.antlr.v4.runtime._
import cats.implicits._

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuffer

object TesslaSyntaxToTessla extends TranslationPhase[Seq[TesslaParser.ParseResult], Tessla.Specification] {
  override def translate(spec: Seq[TesslaParser.ParseResult]): TranslationPhase.Result[Tessla.Specification] = {
    new TesslaSyntaxToTessla(spec).translate()
  }
}

/**
 * Translation phase which performs the following tasks:
 *   - Translates the parse tree into a TeSSLa AST
 *   - Extracts location information from the tokens.
 */

class TesslaSyntaxToTessla(spec: Seq[TesslaParser.ParseResult])
    extends TranslationPhase.Translator[Tessla.Specification]
    with TesslaParser.CanParseConstantString {
  override def translateSpec(): Tessla.Specification = {
    val statements =
      spec.flatMap { res =>
        res.tree.includes.forEach { include =>
          warnLonelyTesslaDoc(include.tessladoc)
        }
        res.tree.entries.asScala.flatMap(translateEntry(_, res.tokens))
      }
    checkForDuplicates(statements.flatMap(Tessla.getId))
    checkForDuplicates(statements.flatMap(getTypeDefID))
    checkForDuplicates(statements.flatMap(getAnnotationID(_, global = false)))
    checkForDuplicates(statements.flatMap(getAnnotationID(_, global = true)))
    Tessla.Specification(statements)
  }

  private def warnLonelyTesslaDoc(tokens: java.util.List[Token]): Unit = {
    if (tokens.size() > 0) {
      val loc = tokens.asScala.map(Location.fromToken).reduce((a, b) => a.merge(b))
      warn(loc, "Nothing to document here.")
    }
  }

  def getTypeDefID(stat: Tessla.Statement): Option[Tessla.Identifier] = stat match {
    case typeDef: Tessla.TypeDefinition => Some(typeDef.id)
    case _                              => None
  }

  def getAnnotationID(stat: Tessla.Statement, global: Boolean): Option[Tessla.Identifier] = stat match {
    case annoDef: Tessla.AnnotationDefinition if annoDef.global == global =>
      Some(annoDef.id)
    case _ => None
  }

  trait TesslaVisitor[T] extends TesslaSyntaxBaseVisitor[T] {
    final override def visitChildren(node: RuleNode) = {
      throw InternalError(
        "Undefined visitor method",
        Location.fromNode(node.asInstanceOf[ParserRuleContext])
      )
    }
  }

  def translateEntry(entry: TesslaSyntax.EntryContext, tokens: CommonTokenStream): Option[Tessla.Statement] = {
    new EntryVisitor(tokens).visit(entry)
  }

  def translateStatement(stat: TesslaSyntax.StatementContext, tokens: CommonTokenStream): Tessla.Statement = {
    new StatementVisitor(tokens).visit(stat.statemnt())
  }

  def translateDefinition(definition: TesslaSyntax.DefContext): Tessla.Definition = {
    val liftable = definition.header.liftable != null
    val typeParameters = definition.header.typeParameters.asScala.map(mkID)
    checkForDuplicates(typeParameters.toSeq)
    val parameters = definition.header.parameters.asScala.map(translateParameter).toSeq
    val paramIDs = parameters.map(_._2.id)
    checkForDuplicates(paramIDs)
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
      definition.header.paren != null,
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

  def translateAnnotation(annotation: TesslaSyntax.AnnotationContext): Tessla.Annotation = {
    translateAnnotationInner(annotation.annotationInner(), annotation)
  }

  def translateAnnotationInner(
    inner: TesslaSyntax.AnnotationInnerContext,
    parent: ParserRuleContext
  ): Tessla.Annotation = {
    val args = inner.arguments.asScala.map { arg =>
      val a = translateExpression(arg.expression)
      if (arg.name != null) {
        Tessla.NamedArgument(mkID(arg.name), a)
      } else {
        Tessla.PositionalArgument(a)
      }.asInstanceOf[Tessla.Argument[Tessla.Expression]]
    }
    Tessla.Annotation(mkID(inner.ID), args.toSeq, Location.fromNode(parent))
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

  class EntryVisitor(tokens: CommonTokenStream) extends TesslaVisitor[Option[Tessla.Statement]] {
    override def visitStatement(ctx: TesslaSyntax.StatementContext) = Some(translateStatement(ctx, tokens))

    override def visitLonelyTesslaDoc(ctx: TesslaSyntax.LonelyTesslaDocContext) = {
      warnLonelyTesslaDoc(ctx.tessladoc)
      None
    }
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
      val isGlobal = annotationDef.AT() == null
      Tessla.AnnotationDefinition(mkID(annotationDef.ID), params.toSeq.map(_._2), isGlobal, loc)
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
      val contents = module.contents.asScala.flatMap(translateEntry(_, tokens))
      Tessla.Module(mkID(module.name), contents.toSeq, Location.fromNode(module))
    }

    override def visitImportStatement(imprt: TesslaSyntax.ImportStatementContext): Tessla.Statement = {
      Tessla.Import(imprt.path.asScala.map(mkID).toList, Location.fromNode(imprt))
    }

    override def visitGlobalAnnotationStatement(annotation: TesslaSyntax.GlobalAnnotationStatementContext) = {
      Tessla.GlobalAnnotation(translateAnnotationInner(annotation.globalAnnotation().annotationInner(), annotation))
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
      Tessla.ObjectType(memberTypes.toMap, Location.fromNode(typ))
    }

    override def visitTupleType(typ: TesslaSyntax.TupleTypeContext) = {
      val types = typ.elementTypes.asScala.map(translateType)
      Tessla.ObjectType(tupleToObject(types.toSeq), Location.fromNode(typ))
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
      val func = translateExpression(funCall.function)
      Tessla.MacroCall(func, typeArgs.toSeq, args.toSeq, loc)
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
