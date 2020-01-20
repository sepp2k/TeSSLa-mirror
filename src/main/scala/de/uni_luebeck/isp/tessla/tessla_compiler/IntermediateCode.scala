package de.uni_luebeck.isp.tessla.tessla_compiler

/**
  * Class containing subclasses for the representation of abstract imperative code which can afterwards be
  * transduced into Java/Rust/... code
  */
object IntermediateCode {

  /**
    * Container class for the translated abstract imperative code
    * @param stepSource Imperative statements executed when a new timestamp arrives
    */

  case class SourceListing(stepSource: Seq[ImpLanStmt],
                           tsGenSource: Seq[ImpLanStmt],
                           inputProcessing: Seq[ImpLanStmt],
                           staticSource: Seq[ImpLanStmt]) {

  }

  sealed trait ImpLanType

  sealed trait ImpLanStmt

  sealed trait ImpLanExpr extends ImpLanStmt

  sealed trait ImpLanVal extends ImpLanExpr

  final case object LongType extends ImpLanType {
    override def toString = "Long"
  }

  final case object DoubleType extends ImpLanType {
    override def toString = "Double"
  }

  final case object BoolType extends ImpLanType {
    override def toString = "Bool"
  }

  final case object UnitType extends ImpLanType {
    override def toString = "Unit"
  }

  final case object StringType extends ImpLanType {
    override def toString = "String"
  }

  final case class OptionType(valType: ImpLanType) extends ImpLanType {
    override def toString = s"Option<$valType>"
  }

  final case class MutableSetType(valType: ImpLanType) extends ImpLanType {
    override def toString = s"MutSet<$valType>"
  }

  final case class ImmutableSetType(valType: ImpLanType) extends ImpLanType {
    override def toString = s"ImmutSet<$valType>"
  }

  final case class MutableMapType(keyType: ImpLanType, valType: ImpLanType) extends ImpLanType {
    override def toString = s"MutMap<$keyType, $valType>"
  }

  final case class ImmutableMapType(keyType: ImpLanType, valType: ImpLanType) extends ImpLanType {
    override def toString = s"ImmutMap<$keyType, $valType>"
  }

  final case class MutableListType(valType: ImpLanType) extends ImpLanType {
    override def toString = s"MutList<$valType>"
  }

  final case class ImmutableListType(valType: ImpLanType) extends ImpLanType {
    override def toString = s"ImmutList<$valType>"
  }

  final case class FunctionType(argsTypes: Seq[ImpLanType], retType: ImpLanType) extends ImpLanType {
    override def toString = s"${argsTypes.mkString(" x ")} -> $retType"
  }

  final case class LongValue(value: Long) extends ImpLanVal {
    override def toString = value.toString
  }

  final case class DoubleValue(value: Double) extends ImpLanVal {
    override def toString = value.toString + "d"
  }

  final case class BoolValue(value: Boolean) extends ImpLanVal {
    override def toString = value.toString
  }

  final case object UnitValue extends ImpLanVal {
    override def toString = "()"
  }

  final case class StringValue(value: String) extends ImpLanVal {
    override def toString = value.toString
  }

  final case class None(typeHint: ImpLanType) extends ImpLanVal {
    override def toString = s"None[[type: $typeHint]]"
  }

  final case class Some(content: ImpLanVal) extends ImpLanVal {
    override def toString = s"Some($content)"
  }

  final case class EmptyMutableSet(valType: ImpLanType) extends ImpLanVal {
    override def toString = s"MutSet<$valType>{}"
  }

  final case class EmptyImmutableSet(valType: ImpLanType) extends ImpLanVal {
    override def toString = s"ImmutSet<$valType>{}"
  }

  final case class EmptyMutableMap(keyType: ImpLanType, valType: ImpLanType) extends ImpLanVal {
    override def toString = s"MutMap<$keyType, $valType>{}"
  }

  final case class EmptyImmutableMap(keyType: ImpLanType, valType: ImpLanType) extends ImpLanVal {
    override def toString = s"ImmutMap<$keyType, $valType>{}"
  }

  final case class EmptyMutableList(valType: ImpLanType) extends ImpLanVal {
    override def toString = s"MutList<$valType>{}"
  }

  final case class EmptyImmutableList(valType: ImpLanType) extends ImpLanVal {
    override def toString = s"ImmutList<$valType>{}"
  }

  final case class If(guard: Seq[Seq[ImpLanExpr]], stmts: Seq[ImpLanStmt], elseStmts: Seq[ImpLanStmt])
    extends ImpLanStmt {
    override def toString = s"If $guard :\n${stmts.mkString("\n")}\nElse :\n${elseStmts.mkString("\n")}\nEndIf"
  }

  final case class Assignment(lhs: Variable, rexpr: ImpLanExpr, defVal: ImpLanVal, typ: ImpLanType)
    extends ImpLanStmt {
    override def toString = s"$lhs = ${rexpr} (default: ${defVal})"
  }

  final case class FinalAssignment(lhs: Variable, defVal: ImpLanVal, typ: ImpLanType) extends ImpLanStmt {
    override def toString = s"$lhs = ${defVal} (final)"
  }

  final case class ReturnStatement(expr: ImpLanExpr) extends ImpLanStmt {
    override def toString = s"return ${expr}"
  }

  final case class FunctionCall(name: String, params: Seq[ImpLanExpr], typeHint: FunctionType) extends ImpLanExpr {
    override def toString = s"$name( ${params.mkString(", ")} ) [[type: $typeHint]]"
  }

  final case class FunctionVarApplication(variable: Variable, params: Seq[ImpLanExpr]) extends ImpLanExpr {
    override def toString = s"$variable( ${params.mkString(", ")} )"
  }

  final case class Addition(op1: ImpLanExpr, op2: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"$op1 + $op2"
  }

  final case class Subtraction(op1: ImpLanExpr, op2: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"$op1 - $op2"
  }

  final case class BitwiseOr(op1: ImpLanExpr, op2: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"$op1 | $op2"
  }

  final case class TernaryExpression(guard: Seq[Seq[ImpLanExpr]], e1: ImpLanExpr, e2: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"$guard ? $e1 : $e2"
  }

  final case class Equal(a: ImpLanExpr, b: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"$a == $b"
  }

  final case class NotEqual(a: ImpLanExpr, b: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"$a != $b"
  }

  final case class Greater(a: ImpLanExpr, b: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"$a > $b"
  }

  final case class GreaterEqual(a: ImpLanExpr, b: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"$a >= $b"
  }

  final case class Negation(a: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"!($a)"
  }

  final case class Variable(name: String) extends ImpLanExpr {
    override def toString = name
  }

  final case class LambdaExpression(argNames: Seq[String], argsTypes: Seq[ImpLanType], retType: ImpLanType, body: Seq[ImpLanStmt]) extends ImpLanExpr {
    override def toString = s"${argsTypes.zip(argNames).mkString(" x ")} -> $retType {\n${body.mkString("\n")}\n}"
  }

}
