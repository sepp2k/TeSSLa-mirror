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
    val body = translateExpression(definition.body)
    Tessla.Definition(
      annotations, mkID(definition.header.name), typeParameters, parameters,
      Option(definition.header.resultType).map(translateType),
      Location.fromNode(definition.header), body, Location.fromNode(definition)
    )
  }

  object StatementVisitor extends TesslaVisitor[Tessla.Statement] {
    override def visitDefinition(definition: TesslaSyntax.DefinitionContext) = {
      translateDefinition(definition.`def`)
    }

    override def visitOut(out: TesslaSyntax.OutContext) = {
      Tessla.Out(translateExpression(out.expression), Option(out.ID).map(mkID), Location.fromNode(out))
    }

    override def visitOutAll(outAll: TesslaSyntax.OutAllContext) = {
      Tessla.OutAll(Location.fromNode(outAll))
    }

    override def visitIn(in: TesslaSyntax.InContext) = {
      Tessla.In(mkID(in.ID), translateType(in.`type`), Location.fromNode(in))
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

    override def visitFunctionType(ctx: TesslaSyntax.FunctionTypeContext) = ???

    override def visitObjectType(ctx: TesslaSyntax.ObjectTypeContext) = ???
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

    override def visitParenthesizedExpression(exp: TesslaSyntax.ParenthesizedExpressionContext) = {
      visit(exp.inner)
    }

    override def visitBlock(block: TesslaSyntax.BlockContext) = {
      val defs = block.definitions.asScala.map(translateDefinition)
      Tessla.Block(defs, translateExpression(block.expression), Location.fromNode(block))
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
      val elseCase = translateExpression(ite.elseCase)
      val loc = Location.fromNode(ite)
      if (ite.staticModifier != null) {
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

    override def visitMemberAccess(ma: TesslaSyntax.MemberAccessContext) = {
      Tessla.MemberAccess(translateExpression(ma.obj), mkID(ma.fieldName), Location.fromNode(ma))
    }

    override def visitIntLiteral(intLit: TesslaSyntax.IntLiteralContext) = {
      if (intLit.DECINT != null) {
        Tessla.Literal(Tessla.IntLiteral(intLit.DECINT.getText.toInt), Location.fromNode(intLit))
      } else {
        require(intLit.HEXINT != null)
        require(intLit.HEXINT.getText.startsWith("0x"))
        val i = Integer.parseInt(intLit.HEXINT.getText.substring(2), 16)
        Tessla.Literal(Tessla.IntLiteral(i), Location.fromNode(intLit))
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
