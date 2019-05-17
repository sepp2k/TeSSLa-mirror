package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.InputTraceParser._
import de.uni_luebeck.isp.tessla.util.Lazy
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.RuleNode

import scala.collection.mutable
import scala.collection.JavaConverters._

class FlatEventIterator(eventRanges: Iterator[EventRangeContext], abortAt: Option[BigInt]) extends Iterator[Trace.Event] {
  val queue: mutable.PriorityQueue[EventRange] =
    new mutable.PriorityQueue[EventRange]()(Ordering.by((ev: EventRange) => ev.from).reverse)
  var nextEvents = new mutable.Queue[Trace.Event]
  var eventCounter = 0

  case class EventRange(from: BigInt, to: Option[BigInt], step: BigInt, t: Trace.Identifier, rangeLoc: Location,
                        streamName: Trace.Identifier, exp: Option[ExpressionContext], loc: Location) {
    def evalValue: TesslaCore.Value = {
      exp.map(ExpressionVisitor.visit).getOrElse(TesslaCore.TesslaObject(Map(), loc))
    }

    object ExpressionVisitor extends InputTraceParserBaseVisitor[TesslaCore.Value] {
      override def visitVariable(variable: VariableContext) = {
        val varName = variable.ID.getText
        val loc = Location.fromNode(variable)
        lookupVar(varName, loc)
      }

      def lookupVar(varName: String, loc: Location) = {
        if (varName == t.name) {
          TesslaCore.IntValue(from, loc)
        } else {
          throw UndefinedVariable(Tessla.Identifier(varName, loc))
        }
      }

      override def visitTrue(ctx: TrueContext) = TesslaCore.BoolValue(true, Location.fromNode(ctx))

      override def visitFalse(ctx: FalseContext) = TesslaCore.BoolValue(false, Location.fromNode(ctx))

      override def visitIntLiteral(intLit: IntLiteralContext) = {
        if (intLit.DECINT != null) {
          TesslaCore.IntValue(BigInt(intLit.DECINT.getText), Location.fromNode(intLit))
        } else {
          require(intLit.HEXINT != null)
          require(intLit.HEXINT.getText.startsWith("0x"))
          TesslaCore.IntValue(BigInt(intLit.HEXINT.getText.substring(2), 16), Location.fromNode(intLit))
        }
      }

      override def visitFloatLiteral(floatLit: FloatLiteralContext) = {
        TesslaCore.FloatValue(floatLit.FLOAT.getText.toDouble, Location.fromNode(floatLit))
      }

      def parseEscapeSequence(sequence: String, loc: Location): String = {
        TesslaParser.parseEscapeSequence(sequence).getOrElse{
          throw InvalidEscapeSequence(sequence, loc)
        }
      }

      override def visitStringLiteral(str: StringLiteralContext) = {
        val result = new StringBuilder
        str.stringContents.forEach {part =>
          if (part.TEXT != null) {
            result ++= part.TEXT.getText
          } else if (part.ESCAPE_SEQUENCE != null) {
            val partLoc = Location.fromNode(part)
            result ++= parseEscapeSequence(part.ESCAPE_SEQUENCE.getText, partLoc)
          } else {
            val value =
              if (part.ID != null) {
                lookupVar(part.ID.getText, Location.fromNode(part))
              } else {
                visit(part.expression())
              }
            result ++= Evaluator.evalToString(value)
          }
        }
        TesslaCore.StringValue(result.toString, Location.fromNode(str))
      }

      override def visitNone(none: NoneContext) = {
        TesslaCore.TesslaOption(None, Location.fromNode(none))
      }

      override def visitSome(some: SomeContext) = {
        TesslaCore.TesslaOption(Some(visit(some.expression)), Location.fromNode(some))
      }

      override def visitListExpression(list: ListExpressionContext) = {
        val elements = list.elems.asScala.map(visit)
        TesslaCore.TesslaList(elements.toIndexedSeq, Location.fromNode(list))
      }

      override def visitSetExpression(set: SetExpressionContext) = {
        val elements = set.elems.asScala.map(visit).toSet
        TesslaCore.TesslaSet(elements, Location.fromNode(set))
      }

      override def visitMapExpression(map: MapExpressionContext) = {
        val elements = map.elems.asScala.map(kv => visit(kv.key) -> visit(kv.value)).toMap
        TesslaCore.TesslaMap(elements, Location.fromNode(map))
      }

      override def visitTupleExpression(tup: TupleExpressionContext) = {
        if (tup.elems.size == 1 && tup.lastComma == null) {
          visit(tup.elems.get(0))
        } else {
          val values = tup.elems.asScala.map(visit)
          val members = values.zipWithIndex.map {
            case (value, index) => s"_${index+1}" -> value
          }.toMap
          TesslaCore.TesslaObject(members, Location.fromNode(tup))
        }
      }

      override def visitObjectLiteral(obj: ObjectLiteralContext) = {
        val members = obj.members.asScala.map { memberDef =>
          memberDef.ID.getText -> visit(memberDef.expression)
        }.toMap
        TesslaCore.TesslaObject(members, Location.fromNode(obj))
      }

      def getOperator(name: String, loc: Location): TesslaCore.BuiltInOperator = {
        TesslaCore.BuiltInOperator(name, loc)
      }

      override def visitUnaryExpression(exp: UnaryExpressionContext) = {
        val operatorName = s"unary ${exp.op.getText}"
        val loc = Location.fromNode(exp)
        val opLoc = Location.fromToken(exp.op)
        Evaluator.evalApplication(getOperator(operatorName, opLoc), Seq(visit(exp.expression)), loc).forceValue
      }

      override def visitInfixExpression(exp: InfixExpressionContext) = {
        val args = Seq(visit(exp.lhs), visit(exp.rhs))
        val loc = Location.fromNode(exp)
        val opLoc = Location.fromToken(exp.op)
        Evaluator.evalApplication(getOperator(exp.op.getText, opLoc), args, loc).forceValue
      }

      override def visitITE(ite: ITEContext) = {
        val loc = Location.fromNode(ite)
        Evaluator.evalIfThenElse(visit(ite.condition), Lazy(visit(ite.thenCase)), Lazy(visit(ite.elseCase)),
          Map(), loc).forceValue
      }

      override final def visitChildren(node: RuleNode): TesslaCore.Value = {
        throw InternalError("Undefined visitor method", Location.fromNode(node.asInstanceOf[ParserRuleContext]))
      }
    }
  }

  object EventRange {
    def apply(erc: EventRangeContext): EventRange = {
      val id = Trace.Identifier(erc.streamName.getText, Location.fromToken(erc.ID))
      val t = Trace.Identifier("t", Location.builtIn)
      val exp = Option(erc.expression)
      val trLoc = Location.fromNode(erc.timeRange)
      val loc = Location.fromNode(erc)
      erc.timeRange match {
        case stc: SingleTimeContext =>
          val time = BigInt(stc.DECINT.getText)
          new EventRange(time, Some(time), 1, t, trLoc, id, exp, loc)
        case crc: CompRangeContext =>
          val lower =
            if (crc.lowerOp.getText == "<") BigInt(crc.lowerBound.getText) + 1
            else BigInt(crc.lowerBound.getText)
          val upper = Option(crc.upperBound).map {bound =>
            if (crc.upperOp.getText == "<") BigInt(bound.getText) - 1
            else BigInt(bound.getText)
          }
          val tID = Trace.Identifier(crc.ID.getText, Location.fromToken(crc.ID))
          EventRange(lower, upper, 1, tID, trLoc, id, exp, loc)
        case rc: RangeContext =>
          val first = BigInt(rc.first.getText)
          val step =
            if (rc.second != null) BigInt(rc.second.getText) - first
            else BigInt(1)
          val last = Option(rc.last).map(l => BigInt(l.getText))
          EventRange(first, last, step, t, trLoc, id, exp, loc)
      }
    }
  }

  private def generateEvent: Option[Trace.Event] = {
    if (queue.nonEmpty) {
      val generator = queue.dequeue
      if (generator.step <= 0) {
        throw NonPositiveStepError(generator.step, generator.loc)
      }
      val diff = generator.to.map(_ - generator.from)
      if (diff.forall(_ >= 0) && abortAt.forall(eventCounter <= _)) {
        if (diff.forall(_ >= generator.step)) {
          queue.enqueue(generator.copy(from = generator.from + generator.step))
        }
        eventCounter += 1
        val ts = Trace.TimeStamp(generator.rangeLoc, generator.from)
        Some(Trace.Event(generator.loc, ts, Some(generator.streamName), generator.evalValue))
      } else {
        None
      }
    } else {
      None
    }
  }

  def gatherValues(): Unit = {
    while (abortAt.forall(eventCounter <= _) && nextEvents.isEmpty && (queue.nonEmpty || eventRanges.hasNext)) {
      if (eventRanges.hasNext) {
        queue.enqueue(EventRange(eventRanges.next))
      }
      nextEvents ++= generateEvent.toList
    }
  }

  override def hasNext = {
    gatherValues()
    nextEvents.nonEmpty
  }

  override def next = {
    gatherValues()
    nextEvents.dequeue
  }
}
