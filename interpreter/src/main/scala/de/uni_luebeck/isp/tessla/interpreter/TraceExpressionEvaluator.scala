/*

 */

package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.core.Errors.{InternalError, InvalidEscapeSequence}
import de.uni_luebeck.isp.tessla.core.InputTraceParser._
import de.uni_luebeck.isp.tessla.core.util.Lazy
import de.uni_luebeck.isp.tessla.core._
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.RuleNode

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._

/**
 * Evaluates expressions used in traces.
 *
  * @see [[de.uni_luebeck.isp.tessla.interpreter.TraceParser.Event]]
 */

object TraceExpressionEvaluator {

  def eval(exp: ExpressionContext): Any = {
    Option(exp).map(ExpressionVisitor.visit).getOrElse(ConstantEvaluator.Record(Map()))
  }

  private object ExpressionVisitor extends InputTraceParserBaseVisitor[Any] {
    private val externs = RuntimeExterns.runtimeCommonExterns

    override def visitTrue(ctx: TrueContext) = true

    override def visitFalse(ctx: FalseContext) = false

    override def visitIntLiteral(intLit: IntLiteralContext): BigInt = {
      if (intLit.DECINT != null) {
        BigInt(intLit.DECINT.getText)
      } else {
        require(intLit.HEXINT != null)
        require(intLit.HEXINT.getText.startsWith("0x"))
        BigInt(intLit.HEXINT.getText.substring(2), 16)
      }
    }

    override def visitFloatLiteral(floatLit: FloatLiteralContext): Double = {
      floatLit.FLOAT.getText.toDouble
    }

    def parseEscapeSequence(sequence: String, loc: Location): String = {
      TesslaParser.parseEscapeSequence(sequence).getOrElse {
        throw InvalidEscapeSequence(sequence, loc)
      }
    }

    override def visitStringLiteral(str: StringLiteralContext): String = {
      val result = new StringBuilder
      str.stringContents.forEach { part =>
        if (part.TEXT != null) {
          result ++= part.TEXT.getText
        } else if (part.ESCAPE_SEQUENCE != null) {
          val partLoc = Location.fromNode(part)
          result ++= parseEscapeSequence(part.ESCAPE_SEQUENCE.getText, partLoc)
        } else {
          val value = visit(part.expression())
          result ++= value.toString
        }
      }
      result.toString
    }

    override def visitNone(none: NoneContext): Option[Any] = {
      // The type of values generated from input streams is never used anywhere, so we can just use a random type
      // We'll use Never here
      // TODO: Think about creating a separate class family for runtime values (as opposed to values that are part
      //       of the TesslaCore AST) that don't have type information attached
      None
    }

    override def visitSome(some: SomeContext): Option[Any] = {
      Some(visit(some.expression))
    }

    override def visitListExpression(list: ListExpressionContext): Seq[Any] = {
      val elements = list.elems.asScala.map(visit)
      elements.toList // TODO: consider IndexedSeq instead?
    }

    override def visitSetExpression(set: SetExpressionContext): Set[Any] = {
      set.elems.asScala.map(visit).toSet
    }

    override def visitMapExpression(map: MapExpressionContext): Map[Any, Any] = {
      map.elems.asScala.map(kv => visit(kv.key) -> visit(kv.value)).toMap
    }

    override def visitTupleExpression(tup: TupleExpressionContext): Any = {
      if (tup.elems.size == 1 && tup.lastComma == null) {
        visit(tup.elems.get(0))
      } else {
        val values = tup.elems.asScala.map(visit)
        val members = values.zipWithIndex.map {
          case (value, index) => s"_${index + 1}" -> value
        }.toMap
        ConstantEvaluator.Record(members)
      }
    }

    override def visitObjectLiteral(obj: ObjectLiteralContext): ConstantEvaluator.Record = {
      val members = obj.members.asScala.map { memberDef =>
        memberDef.ID.getText -> visit(memberDef.expression)
      }.toMap
      ConstantEvaluator.Record(members)
    }

    override def visitUnaryExpression(exp: UnaryExpressionContext): Any = {
      val operatorName = Tessla.unaryOperators(exp.op.getText)
      val args = ArraySeq(Lazy(visit(exp.expression())))
      externs(operatorName).asInstanceOf[RuntimeExterns.RuntimeExtern](args).get
    }

    override def visitInfixExpression(exp: InfixExpressionContext): Any = {
      val operatorName = Tessla.binaryOperators(exp.op.getText)
      val args = ArraySeq(Lazy(visit(exp.lhs)), Lazy(visit(exp.rhs)))
      externs(operatorName).asInstanceOf[RuntimeExterns.RuntimeExtern](args).get
    }

    override def visitITE(ite: ITEContext): Any = {
      externs("ite")
        .asInstanceOf[RuntimeExterns.RuntimeExtern](
          ArraySeq(Lazy(visit(ite.condition)), Lazy(visit(ite.thenCase)), Lazy(visit(ite.elseCase)))
        )
        .get
    }

    final override def visitChildren(node: RuleNode): Any = {
      // $COVERAGE-OFF$
      throw InternalError("Undefined visitor method", Location.fromNode(node.asInstanceOf[ParserRuleContext]))
      // $COVERAGE-ON$
    }
  }

}
