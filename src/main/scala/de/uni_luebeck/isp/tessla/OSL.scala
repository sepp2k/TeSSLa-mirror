package de.uni_luebeck.isp.tessla

import scala.collection.mutable

case class OSL(statements: Seq[OSL.Statement]) {
  override def toString = statements.mkString("\n")
}

object OSL {
  sealed abstract class Statement

  case class If(condition: Condition, thenCase: Logs) extends Statement {
    override def toString = {
      s"if $condition then\n$thenCase\nfi"
    }
  }

  case class Logs(properties: Set[String]) extends Statement {
    override def toString = properties.map(p => s"log $p").mkString("\n")
  }

  sealed abstract class Condition {
    def properties: Set[String]
  }

  case class Equals(property: String, value: TesslaCore.Value) extends Condition {
    override def toString = s"[$property: $value]"

    override lazy val properties = Set(property)
  }

  case class And(lhs: Condition, rhs: Condition) extends Condition {
    override def toString = s"&($lhs, $rhs)"

    override lazy val properties = lhs.properties ++ rhs.properties
  }

  case class Or(lhs: Condition, rhs: Condition) extends Condition {
    override def toString = s"|($lhs, $rhs)"

    override lazy val properties = lhs.properties ++ rhs.properties
  }

  class Generator extends TranslationPhase[TesslaCore.Specification, OSL] {
    var streams: Map[TesslaCore.Identifier, TesslaCore.StreamDefinition] = _
    val visited = mutable.Set[TesslaCore.Identifier]()

    sealed abstract class ProtoStatement
    case class Uncond(property: String) extends ProtoStatement
    case class Cond(conditions: Condition) extends ProtoStatement

    override def translateSpec(spec: TesslaCore.Specification) = {
      streams = spec.streams.toMap
      val protoStatements = spec.outStreams.map(_._2).flatMap(translateStreamRef)
      val mergedConditions = protoStatements.foldLeft(Map[Option[String], Set[String]]()) {
        case (m, Cond(cond)) =>
          val props = cond.properties
          props.foldLeft(m) { (m, prop) =>
              m + (Some(prop) -> (m.getOrElse(Some(prop), Set()) ++ props))
          }
        case (m, Uncond(prop)) =>
          m + (None -> (m.getOrElse(None, Set[String]()) + prop))
      }
      var alreadyUncond = false
      val statements = protoStatements.flatMap {
        case Cond(cond) =>
          Some(If(cond, Logs(mergedConditions(Some(cond.properties.head)))))
        case Uncond(_) =>
          if (alreadyUncond) None
          else {
            alreadyUncond = true
            Some(Logs(mergedConditions(None)))
          }
      }
      OSL(statements)
    }

    def translateStreamRef(streamRef: TesslaCore.StreamRef): Seq[ProtoStatement] = streamRef match {
      case _: TesslaCore.Nil => Seq()
      case is: TesslaCore.InputStream => translateInputStreamName(is.name).map(name => Uncond(name)).toSeq
      case s: TesslaCore.Stream =>
        if (visited(s.id)) Seq()
        else {
          visited += s.id
          val exp = streams(s.id).expression
          findBasicCondition(exp).map(c => Seq(Cond(c))).getOrElse(translateExpression(exp))
        }
    }

    def getExp(streamRef: TesslaCore.StreamRef): Option[TesslaCore.Expression] = streamRef match {
      case _: TesslaCore.Nil => None
      case _: TesslaCore.InputStream => None
      case s: TesslaCore.Stream =>
        Some(streams(s.id).expression)
    }

    def findBasicCondition(streamRef: TesslaCore.StreamRef): Option[Condition] = {
      getExp(streamRef).flatMap(findBasicCondition)
    }

    def findBasicCondition(exp: TesslaCore.Expression): Option[Condition] = exp match {
      case l: TesslaCore.Lift =>
        l.operator match {
          case BuiltIn.And =>
            findBasicCondition(l.args(0)).flatMap { lhs =>
              findBasicCondition(l.args(1)).map { rhs =>
                And(lhs,rhs)
              }
            }
          case BuiltIn.Or =>
            findBasicCondition(l.args(0)).flatMap { lhs =>
              findBasicCondition(l.args(1)).map { rhs =>
                Or(lhs, rhs)
              }
            }
          case BuiltIn.Eq =>
            l.args match {
              case Seq(i: TesslaCore.InputStream, s: TesslaCore.Stream) =>
                translateInputStreamName(i.name).flatMap { name =>
                  getExp(s).flatMap {
                    case TesslaCore.Default(_, v: TesslaCore.Value, _) =>
                      Some(Equals(name, v))
                    case _ => None
                  }
                }
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

    def translateExpression(expression: TesslaCore.Expression): Seq[ProtoStatement] = expression match {
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