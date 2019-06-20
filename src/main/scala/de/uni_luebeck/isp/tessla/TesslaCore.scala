package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{RuntimeError, TesslaError}
import de.uni_luebeck.isp.tessla.util.Lazy
import org.eclipse.tracecompass.ctf.core.event.types.ICompositeDefinition

object TesslaCore extends HasUniqueIdentifiers {
  final case class Specification(streams: Seq[StreamDescription],
                                 inStreams: Seq[InStreamDescription],
                                 outStreams: Seq[OutStreamDescription],
                                 identifierCount: Long) {
    override def toString = {
      inStreams.map { is => s"$is\n" }.mkString +
        streams.map { s => s"$s\n" }.mkString +
        outStreams.map { os => s"$os\n" }.mkString
    }
  }

  type Annotation = TypedTessla.Annotation

  case class StreamDescription(id: Identifier, expression: Expression, typ: StreamType, annotations: Seq[Annotation]) {
    val annotationString = annotations.map { annotation =>
      s"@${annotation.name}(${annotation.arguments.mkString(", ")})\n"
    }.mkString
    override def toString = s"${annotationString}def $id: $typ = $expression"
  }

  case class InStreamDescription(name: String, typ: StreamType, annotations: Seq[Annotation], loc: Location) {
    val annotationString = annotations.map { annotation =>
      s"@${annotation.name}(${annotation.arguments.mkString(", ")})\n"
    }.mkString
    override def toString = s"${annotationString}in $name: $typ"
  }

  case class OutStreamDescription(nameOpt: Option[String], stream: StreamRef, typ: StreamType) {
    override def toString = nameOpt match {
      case Some(name) => s"out $stream: $typ as $name"
      case None => s"print $stream: $typ"
    }
  }

  sealed abstract class Expression {
    def loc: Location
  }

  sealed abstract class Arg

  sealed abstract class StreamRef extends Arg {
    def loc: Location
    def withLoc(loc: Location): StreamRef
  }

  final case class Stream(id: Identifier, loc: Location) extends StreamRef {
    override def toString = id.toString
    def withLoc(loc: Location): Stream = copy(loc = loc)
  }

  final case class InputStream(name: String, loc: Location) extends StreamRef {
    override def toString = s"input($name)"
    def withLoc(loc: Location): InputStream = copy(loc = loc)
  }

  final case class Nil(typ: StreamType, loc: Location) extends StreamRef {
    override def toString = "nil"
    def withLoc(loc: Location): Nil = copy(loc = loc)
  }

  final case class Default(stream: StreamRef, default: ValueOrError, loc: Location) extends Expression {
    override def toString = s"default($stream, $default)"
  }

  final case class DefaultFrom(valueStream: StreamRef, defaultStream: StreamRef, loc: Location) extends Expression {
    override def toString = s"defaultFrom($valueStream, $defaultStream)"
  }

  final case class Time(stream: StreamRef, loc: Location) extends Expression {
    override def toString = s"time($stream)"
  }

  final case class Last(values: StreamRef, clock: StreamRef, loc: Location) extends Expression {
    override def toString = s"last($values, $clock)"
  }

  final case class Delay(delays: StreamRef, resets: StreamRef, loc: Location) extends Expression {
    override def toString = s"delay($delays, $resets)"
  }

  final case class Lift(f: Function, args: Seq[StreamRef], loc: Location) extends Expression {
    override def toString = {
      args.mkString(s"lift($f)(", ", ", ")")
    }
  }

  final case class SignalLift(op: CurriedPrimitiveOperator, args: Seq[StreamRef], loc: Location) extends Expression {
    override def toString = {
      args.mkString(s"slift($op)(", ", ", ")")
    }
  }

  final case class CurriedPrimitiveOperator(name: String, args: Map[Int, ValueOrError] = Map()) {
    override def toString: String = if (args.isEmpty) {
      name
    } else {
      val a = (0 until args.keys.max + 1).map{
        case i if args.contains(i) => args(i).toString
        case _ => "_"
      }.mkString(", ")
      s"$name($a)"
    }
  }

  final case class CustomBuiltInCall(name: String, args: Seq[Arg], loc: Location) extends Expression {
    override def toString = {
      if (args.isEmpty) name
      else args.mkString(s"$name(", ", ", ")")
    }

    def streamArgs: Seq[StreamRef] = args.collect {
      case s: StreamRef => s
    }
  }

  sealed trait ValueArg

  case class ValueExpressionRef(id: Identifier) extends ValueArg {
    override def toString = id.toString
  }

  sealed abstract class ValueExpression {
    def loc: Location
  }

  final case class Function(parameters: Seq[Identifier],
                            body: Map[Identifier, ValueExpression],
                            result: ValueArg,
                            loc: Location) extends ValueExpression {
    override def toString = {
      val defs = body.map {
        case (id, exp) => s"const $id := $exp\n"
      }.mkString
      s"(${parameters.mkString(", ")}) => {\n${defs}return $result\n}"
    }
  }

  case class IfThenElse(cond: ValueArg, thenCase: Lazy[ValueArg], elseCase: Lazy[ValueArg], loc: Location) extends ValueExpression {
    override def toString = s"if $cond then ${thenCase.get} else ${elseCase.get}"
  }

  case class Application(f: Lazy[ValueArg], args: Seq[Lazy[ValueArg]], loc: Location) extends ValueExpression {
    override def toString = args.map(_.get).mkString(s"${f.get}(", ", ", ")")
  }

  // TODO: Hack! This should be a ValueExpression, not a ValueArg
  case class ObjectCreation(members: Map[String, ValueArg], loc: Location) extends ValueArg {
    override def toString = members.map {case (name, value) => s"$name = $value"}.mkString("${", ", ", "}")
  }

  case class MemberAccess(obj: ValueArg, member: String, loc: Location) extends ValueExpression {
    override def toString = s"$obj.$member"
  }

  sealed trait ValueOrError extends Arg with ValueArg {
    def forceValue: Value

    def mapValue(f: Value => ValueOrError): ValueOrError
  }

  final case class Error(error: TesslaError) extends ValueOrError {
    override def forceValue = throw RuntimeError(error)

    override def mapValue(f: Value => ValueOrError) = this
  }

  sealed abstract class Value extends ValueOrError {
    def loc: Location

    def withLoc(loc: Location): Value

    override def forceValue = this

    override def mapValue(f: Value => ValueOrError) = f(this)
  }

  sealed abstract class PrimitiveValue extends Value {
    def value: Any

    override def toString = value.toString

    override def equals(obj: Any) = obj match {
      case other: PrimitiveValue => value == other.value
      case _ => false
    }

    override def hashCode() = value.hashCode()
  }

  final case class IntValue(value: BigInt, loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): IntValue = copy(loc = loc)
  }

  final case class FloatValue(value: Double, loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): FloatValue = copy(loc = loc)
  }

  final case class BoolValue(value: Boolean, loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): BoolValue = copy(loc = loc)
  }

  final case class StringValue(value: String, loc: Location) extends PrimitiveValue {
    override def toString = s""""$value""""
    override def withLoc(loc: Location): StringValue = copy(loc = loc)
  }

  final case class TesslaObject(value: Map[String, ValueOrError], loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): TesslaObject = copy(loc = loc)

    override def toString = {
      val tupleKeys = (1 to value.keys.size).map(i => s"_$i")
      if (value.keys.toSet == tupleKeys.toSet) {
        tupleKeys.map(k => value(k)).mkString("(", ", ", ")")
      } else {
        value.map { case (name, v) => s"$name = $v" }.mkString("{", ", ", "}")
      }
    }
  }

  final case class TesslaOption(value: Option[ValueOrError], loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): TesslaOption = copy(loc = loc)
  }

  final case class TesslaMap(value: Map[Value, Value], loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): TesslaMap = copy(loc = loc)
  }

  final case class TesslaSet(value: Set[Value], loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): TesslaSet = copy(loc = loc)
  }

  final case class TesslaList(value: IndexedSeq[Value], loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): TesslaList = copy(loc = loc)

    override def toString = value.mkString("List(", ", ", ")")
  }

  final case class Ctf(value: ICompositeDefinition, loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): Ctf = copy(loc = loc)
  }

  case class BuiltInOperator(name: String, loc: Location) extends PrimitiveValue {
    override def value = name
    override def withLoc(loc: Location): BuiltInOperator = copy(loc = loc)
  }

  case class Closure(function: Function, var capturedEnvironment: Map[Identifier, Lazy[ValueOrError]], loc: Location) extends Value {
    override def withLoc(loc: Location): Closure = copy(loc = loc)

    override def toString = {
      s"$function (closure)"
    }
  }

  sealed abstract class Type

  sealed abstract class ValueType extends Type

  case class BuiltInType(name: String, typeArgs: Seq[ValueType]) extends ValueType {
    override def toString = {
      if (typeArgs.isEmpty) name
      else typeArgs.mkString(s"$name[", ", ", "]")
    }
  }

  case object FunctionType extends ValueType {
    override def toString = "? => ?"
  }

  case class ObjectType(memberTypes: Map[String, ValueType]) extends ValueType {
    override def toString = {
      val tupleKeys = (1 to memberTypes.keys.size).map(i => s"_$i")
      if (memberTypes.keys.toSet == tupleKeys.toSet) {
        memberTypes.mkString("(", ", ", ")")
      } else {
        memberTypes.map { case (name, t) => s"$name: $t" }.mkString("{", ", ", "}")
      }
    }
  }

  case class StreamType(elementType: ValueType) extends Type {
    override def toString = s"Events[$elementType]"
  }
}
