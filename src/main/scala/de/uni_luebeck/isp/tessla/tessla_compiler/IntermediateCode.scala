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
                           tailSource: Seq[ImpLanStmt],
                           tsGenSource: Seq[ImpLanStmt],
                           callbacks: Seq[ImpLanStmt],
                           staticSource: Seq[ImpLanStmt]) {

    override def toString: String =
      s"## Static Code:\n${staticSource.mkString("\n")}\n" +
      s"## Tail Code:\n${tailSource.mkString("\n")}\n" +
      s"## Step Source:\n${stepSource.mkString("\n")}\n" +
      s"## TS Generation:\n${tsGenSource.mkString("\n")}\n" +
      s"## Callbacks:\n${callbacks.mkString("\n")}\n"

  }

  sealed trait ImpLanType

  abstract class GenericImpLanType(gt: Seq[ImpLanType]) extends ImpLanType {
    val genTypes: Seq[ImpLanType] = gt
  }

  sealed trait ImpLanStmt

  sealed trait ImpLanExpr extends ImpLanStmt

  sealed trait ImpLanVal extends ImpLanExpr

  /* Types */

  final case object GeneralType extends  ImpLanType {
    override def toString = "GeneralType"
  }

  final case object VoidType extends ImpLanType {
    override def toString = "Void"
  }

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


  final case class StructType(subTypes : Seq[ImpLanType], fieldNames: Seq[String]) extends ImpLanType {
    override def toString = s"(${fieldNames.zip(subTypes).map{case (n,t) => s"$n : $t"}.mkString(", ")})"
  }


  final case class OptionType(valType: ImpLanType) extends GenericImpLanType(Seq(valType)) {
    override def toString = s"Option<$valType>"
  }

  final case class MutableSetType(valType: ImpLanType) extends GenericImpLanType(Seq(valType)) {
    override def toString = s"MutSet<$valType>"
  }

  final case class ImmutableSetType(valType: ImpLanType) extends GenericImpLanType(Seq(valType)) {
    override def toString = s"ImmutSet<$valType>"
  }

  final case class MutableMapType(keyType: ImpLanType, valType: ImpLanType) extends GenericImpLanType(Seq(keyType, valType)) {
    override def toString = s"MutMap<$keyType, $valType>"
  }

  final case class ImmutableMapType(keyType: ImpLanType, valType: ImpLanType) extends GenericImpLanType(Seq(keyType, valType)) {
    override def toString = s"ImmutMap<$keyType, $valType>"
  }

  final case class MutableListType(valType: ImpLanType) extends GenericImpLanType(Seq(valType)) {
    override def toString = s"MutList<$valType>"
  }

  final case class ImmutableListType(valType: ImpLanType) extends GenericImpLanType(Seq(valType)) {
    override def toString = s"ImmutList<$valType>"
  }

  final case class FunctionType(argsTypes: Seq[ImpLanType], retType: ImpLanType) extends GenericImpLanType(argsTypes :+ retType) {
    override def toString = s"${argsTypes.mkString(" x ")} -> $retType"
  }

  /* Values */

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

  final case object GeneralValue extends ImpLanVal {
    override def toString = "GeneralValue"
  }

  final case class StructValue(values: Map[String, ImpLanVal]) extends  ImpLanVal {
    override def toString = s"{${values.map{case (k,v) => s"$k: $v)"}.mkString(", ")}}"
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

  final case class EmptyFunction(typeHint: ImpLanType) extends ImpLanVal {
    override def toString = s"(...) -> {}"
  }

  /* Statements */

  final case class If(guard: Seq[Seq[ImpLanExpr]], stmts: Seq[ImpLanStmt], elseStmts: Seq[ImpLanStmt])
    extends ImpLanStmt {
    override def toString = s"If $guard :\n${stmts.mkString("\n")}\nElse :\n${elseStmts.mkString("\n")}\nEndIf"
  }

  final case class TryCatchBlock(tr: Seq[ImpLanStmt], cat: Seq[ImpLanStmt]) extends ImpLanStmt {
    override def toString = s"Try: \n${tr.mkString("\n")}\nCATCH var_err: \n${cat.mkString("\n")}\nEndTry"
  }

  final case class Assignment(lhs: Variable, rexpr: ImpLanExpr, defVal: Option[ImpLanExpr], typ: ImpLanType, global: Boolean)
    extends ImpLanStmt {
    override def toString = s"$lhs = ${rexpr} (default: $defVal, global: $global)"
  }

  final case class FinalAssignment(lhs: Variable, defVal: ImpLanExpr, typ: ImpLanType) extends ImpLanStmt {
    override def toString = s"$lhs = ${defVal} (final)"
  }

  final case class ReturnStatement(expr: ImpLanExpr) extends ImpLanStmt {
    override def toString = s"return ${expr}"
  }

  /* Expressions */

  final case class CastingExpression(e: ImpLanExpr, target: ImpLanType) extends ImpLanExpr {
    override def toString = s"($target)$e"
  }

  final case class FunctionCall(name: String, params: Seq[ImpLanExpr], typeHint: FunctionType) extends ImpLanExpr {
    override def toString = s"$name( ${params.mkString(", ")} ) [[type: $typeHint]]"
  }

  final case class LambdaApplication(exp: ImpLanExpr, params: Seq[ImpLanExpr]) extends ImpLanExpr {
    override def toString = s"$exp( ${params.mkString(", ")} )"
  }

  final case class TernaryExpression(guard: Seq[Seq[ImpLanExpr]], e1: ImpLanExpr, e2: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"$guard ? $e1 : $e2"
  }

  final case class Equal(e1: ImpLanExpr, e2: ImpLanExpr) extends ImpLanExpr {
    override def toString = s"$e1 == $e2"
  }

  final case class Variable(name: String) extends ImpLanExpr {
    override def toString = name
  }

  final case class LambdaExpression(argNames: Seq[String], argsTypes: Seq[ImpLanType], retType: ImpLanType, body: Seq[ImpLanStmt]) extends ImpLanExpr {
    override def toString = s"(${argsTypes.zip(argNames).map{case (a,b) => s"$b : $a"}.mkString(" x ")}) -> $retType {\n${body.mkString("\n")}\n}"
  }

}
