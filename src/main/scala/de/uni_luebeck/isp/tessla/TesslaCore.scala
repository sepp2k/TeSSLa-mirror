package de.uni_luebeck.isp.tessla

object TesslaCore extends HasUniqueIdentifiers {
  final case class StreamDefinition(expression: Expression, typ: Type)

  final case class Specification(streams: Map[Identifier, StreamDefinition],
                                 inStreams: Seq[(Identifier, StreamType, Location)],
                                 outStreams: Seq[(String, StreamRef)]) {
    override def toString = {
      inStreams.map { case (name, typ, _) => s"in $name: $typ\n" }.mkString +
        streams.map { case (name, expr) => s"def $name := $expr\n" }.mkString +
        outStreams.map { case (name, stream) => s"out $stream as $name\n" }.mkString
    }
  }

  sealed abstract class Expression {
    def loc: Location
  }

  sealed abstract class StreamRef {
    def loc: Location
    def withLoc(loc: Location): StreamRef
  }

  final case class Stream(id: Identifier, loc: Location) extends StreamRef {
    override def toString = id.toString
    def withLoc(loc: Location): Stream = copy(loc = loc)
  }

  final case class InputStream(id: Identifier, loc: Location) extends StreamRef {
    override def toString = s"input($id)"
    def withLoc(loc: Location): InputStream = copy(loc = loc)
  }

  final case class Nil(loc: Location) extends StreamRef {
    override def toString = "nil"
    def withLoc(loc: Location): Nil = copy(loc = loc)
  }

  final case class Default(stream: StreamRef, default: Value, loc: Location) extends Expression {
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

  final case class DelayedLast(values: StreamRef, delays: StreamRef, loc: Location) extends Expression {
    override def toString = s"delayedLast($values, $delays)"
  }

  final case class Lift(operator: PrimitiveOperators.PrimitiveOperator, typeArgs: Seq[Type], args: Seq[StreamRef], loc: Location) extends Expression {
    override def toString = operator match {
      case _: PrimitiveOperators.PrefixOperator => s"$operator${args(0)}"
      case _: PrimitiveOperators.InfixOperator => s"${args(0)} $operator ${args(1)}"
      case PrimitiveOperators.IfThen => s"if ${args(0)} then ${args(1)}"
      case PrimitiveOperators.IfThenElse => s"if ${args(0)} then ${args(1)} else ${args(2)}"
      case _ =>
        val targs = typeArgs.mkString("[", ", ", "]")
        args.mkString(s"$operator$targs(", ", ", ")")
    }
  }

  sealed abstract class Value {
    def loc: Location
    def withLoc(loc: Location): Value
    def typ: Type
    def value: Any

    override def toString = value.toString
  }

  final case class IntLiteral(value: BigInt, loc: Location) extends Value {
    override def withLoc(loc: Location): IntLiteral = copy(loc = loc)
    override def typ = IntType
  }

  final case class BoolLiteral(value: Boolean, loc: Location) extends Value {
    override def withLoc(loc: Location): BoolLiteral = copy(loc = loc)
    override def typ = BoolType
  }

  final case class StringLiteral(value: String, loc: Location) extends Value {
    override def toString = s""""$value""""
    override def withLoc(loc: Location): StringLiteral = copy(loc = loc)
    override def typ = StringType
  }

  final case class Unit(loc: Location) extends Value {
    override def value = ()
    override def withLoc(loc: Location): Unit = copy(loc = loc)
    override def typ = UnitType
  }

  final case class TesslaMap(value: Map[Value, Value], typ: MapType, loc: Location) extends Value {
    override def withLoc(loc: Location): TesslaMap = copy(loc = loc)
  }

  final case class TesslaSet(value: Set[Value], typ: SetType, loc: Location) extends Value {
    override def withLoc(loc: Location): TesslaSet = copy(loc = loc)
  }

  sealed abstract class Type

  sealed abstract class ValueType extends Type

  case object IntType extends ValueType {
    override def toString = "Int"
  }

  case object BoolType extends ValueType {
    override def toString = "Bool"
  }

  case object StringType extends ValueType {
    override def toString = "String"
  }

  case object UnitType extends ValueType {
    override def toString = "Unit"
  }

  case class MapType(keyType: ValueType, valueType: ValueType) extends ValueType {
    override def toString = s"Map[$keyType, $valueType]"
  }

  case class SetType(elementType: ValueType) extends ValueType {
    override def toString = s"Set[$elementType]"
  }

  case class StreamType(elementType: ValueType) extends Type {
    override def toString = s"Events[$elementType]"
  }
}
