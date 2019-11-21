package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.InputTraceParser._
import de.uni_luebeck.isp.tessla.util.Lazy
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.RuleNode

import scala.collection.mutable
import scala.collection.JavaConverters._

// TODO: organise usage of RuntimeEvaluator
class FlatEventIterator(eventRanges: Iterator[ParserEventRange], abortAt: Option[BigInt], evaluator: Evaluator) extends Iterator[Trace.Event] {

  val queue: mutable.PriorityQueue[EventRange] =
    new mutable.PriorityQueue[EventRange]()(Ordering.by((ev: EventRange) => ev.from).reverse)
  var nextEvents = new mutable.Queue[Trace.Event]
  var eventCounter = 0

  case class EventRange(from: BigInt, to: Option[BigInt], step: BigInt, t: Trace.Identifier, rangeLoc: Location,
    streamName: Trace.Identifier, exp: Option[ExpressionContext], loc: Location
  ) {
    def evalValue: Any = {
      exp.map(ExpressionVisitor.visit).getOrElse(RuntimeEvaluator.Record(Map()))
    }

    object ExpressionVisitor extends InputTraceParserBaseVisitor[Any] {
      override def visitVariable(variable: VariableContext) = {
        val varName = variable.ID.getText
        val loc = Location.fromNode(variable)
        lookupVar(varName, loc)
      }

      def lookupVar(varName: String, loc: Location) = {
        if (varName == t.name) {
          from
        } else {
          throw UndefinedVariable(Tessla.Identifier(varName, loc))
        }
      }

      override def visitTrue(ctx: TrueContext) = true

      override def visitFalse(ctx: FalseContext) = false

      override def visitIntLiteral(intLit: IntLiteralContext) = {
        if (intLit.DECINT != null) {
          BigInt(intLit.DECINT.getText)
        } else {
          require(intLit.HEXINT != null)
          require(intLit.HEXINT.getText.startsWith("0x"))
          BigInt(intLit.HEXINT.getText.substring(2), 16)
        }
      }

      override def visitFloatLiteral(floatLit: FloatLiteralContext) = {
        floatLit.FLOAT.getText.toDouble
      }

      def parseEscapeSequence(sequence: String, loc: Location): String = {
        TesslaParser.parseEscapeSequence(sequence).getOrElse {
          throw InvalidEscapeSequence(sequence, loc)
        }
      }

      override def visitStringLiteral(str: StringLiteralContext) = {
        val result = new StringBuilder
        str.stringContents.forEach { part =>
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
            result ++= value.toString
          }
        }
        result.toString
      }

      override def visitNone(none: NoneContext) = {
        // The type of values generated from input streams is never used anywhere, so we can just use a random type
        // We'll use Never here
        // TODO: Think about creating a separate class family for runtime values (as opposed to values that are part
        //       of the TesslaCore AST) that don't have type information attached
        None
      }

      override def visitSome(some: SomeContext) = {
        Some(visit(some.expression))
      }

      override def visitListExpression(list: ListExpressionContext) = {
        val elements = list.elems.asScala.map(visit)
        elements.toList // TODO: consider IndexedSeq instead?
      }

      override def visitSetExpression(set: SetExpressionContext) = {
        set.elems.asScala.map(visit).toSet
      }

      override def visitMapExpression(map: MapExpressionContext) = {
        map.elems.asScala.map(kv => visit(kv.key) -> visit(kv.value)).toMap
      }

      override def visitTupleExpression(tup: TupleExpressionContext) = {
        if (tup.elems.size == 1 && tup.lastComma == null) {
          visit(tup.elems.get(0))
        } else {
          val values = tup.elems.asScala.map(visit)
          val members = values.zipWithIndex.map {
            case (value, index) => s"_${index + 1}" -> value
          }.toMap
          RuntimeEvaluator.Record(members)
        }
      }

      override def visitObjectLiteral(obj: ObjectLiteralContext) = {
        val members = obj.members.asScala.map { memberDef =>
          memberDef.ID.getText -> visit(memberDef.expression)
        }.toMap
        RuntimeEvaluator.Record(members)
      }

      def getOperator(name: String, loc: Location): TesslaCore.BuiltInOperator = {
        TesslaCore.BuiltInOperator(name, loc)
      }

      // TODO: reactivate this using RuntimeEvaluator
      override def visitUnaryExpression(exp: UnaryExpressionContext) = {
        val operatorName = Tessla.unaryOperators(exp.op.getText)
        val args = List(Lazy(visit(exp.expression())))
        RuntimeEvaluator.commonExterns(operatorName)(args)
      }

      override def visitInfixExpression(exp: InfixExpressionContext) = {
        val operatorName = Tessla.binaryOperators(exp.op.getText)
        val args = List(Lazy(visit(exp.lhs)), Lazy(visit(exp.rhs)))
        val loc = Location.fromNode(exp)
        val opLoc = Location.fromToken(exp.op)
        RuntimeEvaluator.commonExterns(operatorName)(args)
      }

      override def visitITE(ite: ITEContext) = {
        val loc = Location.fromNode(ite)
        RuntimeEvaluator.commonExterns("ite")(List(Lazy(visit(ite.condition)), Lazy(visit(ite.thenCase)), Lazy(visit(ite.elseCase))))
      }

      override final def visitChildren(node: RuleNode): Any = {
        throw InternalError("Undefined visitor method", Location.fromNode(node.asInstanceOf[ParserRuleContext]))
      }
    }

  }

  object EventRange {
    def apply(parserEventRange: ParserEventRange): EventRange = {
      val id = Trace.Identifier(parserEventRange.streamName.getText, Location.fromToken(parserEventRange.streamName))
      val t = Trace.Identifier("t", Location.builtIn)
      val exp = Option(parserEventRange.expression)
      val trLoc = Location.fromNode(parserEventRange.timeRange)
      parserEventRange.timeRange match {
        case stc: SingleTimeContext =>
          val time = BigInt(stc.DECINT.getText)
          new EventRange(time, Some(time), 1, t, trLoc, id, exp, parserEventRange.loc)
        case crc: CompRangeContext =>
          val lower =
            if (crc.lowerOp.getText == "<") BigInt(crc.lowerBound.getText) + 1
            else BigInt(crc.lowerBound.getText)
          val upper = Option(crc.upperBound).map { bound =>
            if (crc.upperOp.getText == "<") BigInt(bound.getText) - 1
            else BigInt(bound.getText)
          }
          val tID = Trace.Identifier(crc.ID.getText, Location.fromToken(crc.ID))
          EventRange(lower, upper, 1, tID, trLoc, id, exp, parserEventRange.loc)
        case rc: RangeContext =>
          val first = BigInt(rc.first.getText)
          val step =
            if (rc.second != null) BigInt(rc.second.getText) - first
            else BigInt(1)
          val last = Option(rc.last).map(l => BigInt(l.getText))
          EventRange(first, last, step, t, trLoc, id, exp, parserEventRange.loc)
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
