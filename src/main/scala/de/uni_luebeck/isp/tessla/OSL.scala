package de.uni_luebeck.isp.tessla

import scala.collection.mutable

case class OSL(statements: Seq[OSL.Statement]) {
  override def toString = statements.mkString("\n")
}

object OSL {
  sealed abstract class Statement {
    def and(rhs: Statement): Statement = (this, rhs) match {
      case (l1: Logs, l2: Logs) => Logs(l1.properties ++ l2.properties)
      case (l: Logs, i: If) => If(i.condition, Logs(l.properties ++ i.thenCase.properties))
      case (i: If, l: Logs) => If(i.condition, Logs(i.thenCase.properties ++ l.properties))
      case (i1: If, i2: If) => If(And(i1.condition, i2.condition), Logs(i1.thenCase.properties ++ i2.thenCase.properties))
    }

    def or(rhs: Statement): Statement = (this, rhs) match {
      case (l1: Logs, l2: Logs) => Logs(l1.properties ++ l2.properties)
      case (l: Logs, i: If) => If(i.condition, Logs(l.properties ++ i.thenCase.properties))
      case (i: If, l: Logs) => If(i.condition, Logs(i.thenCase.properties ++ l.properties))
      case (i1: If, i2: If) => If(Or(i1.condition, i2.condition), Logs(i1.thenCase.properties ++ i2.thenCase.properties))
    }
  }

  case class If(condition: Condition, thenCase: Logs) extends Statement {
    override def toString = {
      s"if $condition then\n$thenCase\nfi"
    }
  }

  case class Logs(properties: Seq[String]) extends Statement {
    override def toString = properties.map(p => s"log $p").mkString("\n")
  }

  sealed abstract class Condition

  case class Equals(property: String, value: TesslaCore.Value) extends Condition {
    override def toString = s"[$property: $value]"
  }

  case class And(lhs: Condition, rhs: Condition) extends Condition {
    override def toString = s"&($lhs, $rhs)"
  }

  case class Or(lhs: Condition, rhs: Condition) extends Condition {
    override def toString = s"|($lhs, $rhs)"
  }

  class Generator extends TranslationPhase[TesslaCore.Specification, OSL] {
    var streams: Map[TesslaCore.Identifier, TesslaCore.StreamDefinition] = _
    val visited = mutable.Set[TesslaCore.Identifier]()

    override def translateSpec(spec: TesslaCore.Specification) = {
      streams = spec.streams.toMap
      val statements = spec.outStreams.map(_._2).flatMap(translateStreamRef)
      OSL(statements)
    }

    def translateStreamRef(streamRef: TesslaCore.StreamRef): Seq[Statement] = streamRef match {
      case _: TesslaCore.Nil => Seq()
      case is: TesslaCore.InputStream => translateInputStreamName(is.name).map(name => Logs(Seq(name))).toSeq
      case s: TesslaCore.Stream =>
        if (visited(s.id)) Seq()
        else {
          visited += s.id
          val exp = streams(s.id).expression
          findBasicCondition(exp).map(Seq(_)).getOrElse(translateExpression(exp))
        }
    }

    def getExp(streamRef: TesslaCore.StreamRef): Option[TesslaCore.Expression] = streamRef match {
      case _: TesslaCore.Nil => None
      case _: TesslaCore.InputStream => None
      case s: TesslaCore.Stream =>
        Some(streams(s.id).expression)
    }

    def findBasicCondition(streamRef: TesslaCore.StreamRef): Option[Statement] = {
      getExp(streamRef).flatMap(findBasicCondition)
    }

    def findBasicCondition(exp: TesslaCore.Expression): Option[Statement] = exp match {
      case l: TesslaCore.Lift =>
        l.operator match {
          case BuiltIn.And =>
            findBasicCondition(l.args(0)).flatMap { lhs =>
              findBasicCondition(l.args(1)).map { rhs =>
                lhs.and(rhs)
              }
            }
          case BuiltIn.Or =>
            findBasicCondition(l.args(0)).flatMap { lhs =>
              findBasicCondition(l.args(1)).map { rhs =>
                lhs.or(rhs)
              }
            }
          case BuiltIn.Eq =>
            l.args match {
              case Seq(i: TesslaCore.InputStream, s: TesslaCore.Stream) =>
                translateInputStreamName(i.name).flatMap { name =>
                  getExp(s).flatMap {
                    case TesslaCore.Default(_, v: TesslaCore.Value, _) =>
                      Some(If(Equals(name, v), Logs(Seq(name))))
                    case _ => None
                  }
                }
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

    def translateExpression(expression: TesslaCore.Expression): Seq[Statement] = expression match {
      case l: TesslaCore.Lift => l.args.flatMap(translateStreamRef)
      case d: TesslaCore.Default => translateStreamRef(d.stream)
      case d: TesslaCore.DefaultFrom => translateStreamRef(d.valueStream) ++ translateStreamRef(d.defaultStream)
      case l: TesslaCore.Last => translateStreamRef(l.values) ++ translateStreamRef(l.clock)
      case dl: TesslaCore.DelayedLast => translateStreamRef(dl.values) ++ translateStreamRef(dl.delays)
      case m: TesslaCore.Merge => translateStreamRef(m.stream1) ++ translateStreamRef(m.stream2)
      case t: TesslaCore.Time => translateStreamRef(t.stream)
      case c: TesslaCore.Const => translateStreamRef(c.stream)
    }

    val operandPattern = raw"operand\d+(?:type|int|bool|string)|operandcount".r

    def translateInputStreamName(name: String): Option[String] = name match {
      case "line" => Some("lines")
      case "column" => Some("columns")
      case "function" => Some("functions")
      case "functioncall" => Some("function_calls")
      case "instruction" => Some("opcodes")
      case "thread_id" | "line_reached" => Some(name)
      case operandPattern() => Some("operands")
      case _ => None
    }
  }
}