/*
 * Copyright 2020 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.tessla_compiler

/**
 * Class containing subclasses for the representation of abstract imperative code which can afterwards be
 * transduced into Scala/Java/Rust/... code
 */

object IntermediateCode {

  sealed trait ImpLanType

  sealed trait ImpLanStmt

  sealed trait ImpLanExpr extends ImpLanStmt

  sealed trait ImpLanVal extends ImpLanExpr

  sealed abstract class GenericImpLanType(gt: Seq[ImpLanType]) extends ImpLanType {
    val genTypes: Seq[ImpLanType] = gt
  }

  /**
   * Container class for the translated abstract imperative code
   * @param stepSource Imperative statements executed when a new timestamp arrives
   * @param tailSource  Imperative statements executed when a calculation has just been performed
   * @param tsGenSource Imperative statements executed to negotiate the next active timestamp
   * @param inputProcessing Imperative statements executed to process the input
   * @param staticSource Imperative statements executed prior monitor execution
   */
  case class SourceListing(
    stepSource: Seq[ImpLanStmt],
    tailSource: Seq[ImpLanStmt],
    tsGenSource: Seq[ImpLanStmt],
    inputProcessing: Seq[ImpLanStmt],
    staticSource: Seq[ImpLanStmt]
  ) {

    override def toString: String =
      s"## Static Code:\n${staticSource.mkString("\n")}\n" +
        s"## Tail Code:\n${tailSource.mkString("\n")}\n" +
        s"## Step Source:\n${stepSource.mkString("\n")}\n" +
        s"## TS Generation:\n${tsGenSource.mkString("\n")}\n" +
        s"## Input Processing:\n${inputProcessing.mkString("\n")}\n"

    def mapAll(f: Seq[ImpLanStmt] => Seq[ImpLanStmt]): SourceListing = {
      SourceListing(
        f(stepSource),
        f(tailSource),
        f(tsGenSource),
        f(inputProcessing),
        f(staticSource)
      )
    }

  }

  /* Types */

  final case class StructType(subTypes: Seq[ImpLanType], fieldNames: Seq[String]) extends ImpLanType {
    override def toString: String = s"(${fieldNames.zip(subTypes).map { case (n, t) => s"$n : $t" }.mkString(", ")})"
  }

  final case class OptionType(valType: ImpLanType) extends GenericImpLanType(Seq(valType)) {
    override def toString: String = s"Option<$valType>"
  }

  final case class MutableSetType(valType: ImpLanType) extends GenericImpLanType(Seq(valType)) {
    override def toString: String = s"MutSet<$valType>"
  }

  final case class ImmutableSetType(valType: ImpLanType) extends GenericImpLanType(Seq(valType)) {
    override def toString: String = s"ImmutSet<$valType>"
  }

  final case class MutableMapType(keyType: ImpLanType, valType: ImpLanType)
      extends GenericImpLanType(Seq(keyType, valType)) {
    override def toString: String = s"MutMap<$keyType, $valType>"
  }

  final case class ImmutableMapType(keyType: ImpLanType, valType: ImpLanType)
      extends GenericImpLanType(Seq(keyType, valType)) {
    override def toString: String = s"ImmutMap<$keyType, $valType>"
  }

  final case class MutableListType(valType: ImpLanType) extends GenericImpLanType(Seq(valType)) {
    override def toString: String = s"MutList<$valType>"
  }

  final case class ImmutableListType(valType: ImpLanType) extends GenericImpLanType(Seq(valType)) {
    override def toString: String = s"ImmutList<$valType>"
  }

  final case class FunctionType(argsTypes: Seq[ImpLanType], retType: ImpLanType)
      extends GenericImpLanType(argsTypes :+ retType) {
    override def toString: String = s"${argsTypes.mkString(" x ")} -> $retType"
  }

  final case class LazyContainer(subType: ImpLanType) extends GenericImpLanType(Seq(subType)) {
    override def toString: String = s"Lazy[$subType]"
  }

  final case class LongValue(value: Long) extends ImpLanVal {
    override def toString: String = value.toString
  }

  final case class DoubleValue(value: Double) extends ImpLanVal {
    override def toString: String = value.toString + "d"
  }

  final case class BoolValue(value: Boolean) extends ImpLanVal {
    override def toString: String = value.toString
  }

  final case class StructValue(values: Map[String, ImpLanVal]) extends ImpLanVal {
    override def toString: String = s"{${values.map { case (k, v) => s"$k: $v)" }.mkString(", ")}}"
  }

  final case class StringValue(value: String) extends ImpLanVal {
    override def toString: String = value
  }

  final case class NoneValue(typeHint: ImpLanType) extends ImpLanVal {
    override def toString: String = s"None[[type: $typeHint]]"
  }

  final case class SomeValue(content: ImpLanVal) extends ImpLanVal {
    override def toString: String = s"Some($content)"
  }

  /* Values */

  final case class EmptyMutableSet(valType: ImpLanType) extends ImpLanVal {
    override def toString: String = s"MutSet<$valType>{}"
  }

  final case class EmptyImmutableSet(valType: ImpLanType) extends ImpLanVal {
    override def toString: String = s"ImmutSet<$valType>{}"
  }

  final case class EmptyMutableMap(keyType: ImpLanType, valType: ImpLanType) extends ImpLanVal {
    override def toString: String = s"MutMap<$keyType, $valType>{}"
  }

  final case class EmptyImmutableMap(keyType: ImpLanType, valType: ImpLanType) extends ImpLanVal {
    override def toString: String = s"ImmutMap<$keyType, $valType>{}"
  }

  final case class EmptyMutableList(valType: ImpLanType) extends ImpLanVal {
    override def toString: String = s"MutList<$valType>{}"
  }

  final case class EmptyImmutableList(valType: ImpLanType) extends ImpLanVal {
    override def toString: String = s"ImmutList<$valType>{}"
  }

  final case class EmptyFunction(typeHint: ImpLanType) extends ImpLanVal {
    override def toString: String = s"(...) -> {}"
  }

  final case class If(guard: Seq[Seq[ImpLanExpr]], stmts: Seq[ImpLanStmt], elseStmts: Seq[ImpLanStmt])
      extends ImpLanStmt {
    override def toString: String = s"If $guard :\n${stmts.mkString("\n")}\nElse :\n${elseStmts.mkString("\n")}\nEndIf"
  }

  final case class TryCatchBlock(tr: Seq[ImpLanStmt], cat: Seq[ImpLanStmt]) extends ImpLanStmt {
    override def toString: String = s"Try: \n${tr.mkString("\n")}\nCATCH var_err: \n${cat.mkString("\n")}\nEndTry"
  }

  final case class Assignment(lhs: Variable, rexpr: ImpLanExpr, defVal: Option[ImpLanExpr], typ: ImpLanType)
      extends ImpLanStmt {
    override def toString: String = s"$lhs = $rexpr (default: $defVal)"
  }

  final case class FinalAssignment(lhs: Variable, defVal: ImpLanExpr, typ: ImpLanType, lazyVar: Boolean)
      extends ImpLanStmt {
    override def toString: String = (if (lazyVar) "lazy " else "") + s"$lhs = $defVal (final)"
  }

  final case class ReturnStatement(expr: ImpLanExpr) extends ImpLanStmt {
    override def toString: String = s"return $expr"
  }

  final case class CastingExpression(e: ImpLanExpr, from: ImpLanType, target: ImpLanType) extends ImpLanExpr {
    override def toString: String = s"($from -> $target)$e"
  }

  final case class FunctionCall(name: String, params: Seq[ImpLanExpr], typeHint: FunctionType) extends ImpLanExpr {
    override def toString: String = s"$name( ${params.mkString(", ")} ) [[type: $typeHint]]"
  }

  final case class LambdaApplication(exp: ImpLanExpr, params: Seq[ImpLanExpr]) extends ImpLanExpr {
    override def toString: String = s"$exp( ${params.mkString(", ")} )"
  }

  final case class TernaryExpression(guard: Seq[Seq[ImpLanExpr]], e1: ImpLanExpr, e2: ImpLanExpr) extends ImpLanExpr {
    override def toString: String = s"$guard ? $e1 : $e2"
  }

  final case class Equal(e1: ImpLanExpr, e2: ImpLanExpr) extends ImpLanExpr {
    override def toString: String = s"$e1 == $e2"
  }

  /* Statements */

  final case class Variable(name: String) extends ImpLanExpr {
    override def toString: String = name
  }

  final case class LambdaExpression(
    argNames: Seq[String],
    argsTypes: Seq[ImpLanType],
    retType: ImpLanType,
    body: Seq[ImpLanStmt]
  ) extends ImpLanExpr {
    override def toString: String =
      s"(${argsTypes.zip(argNames).map { case (a, b) => s"$b : $a" }.mkString(" x ")}) -> $retType {\n${body.mkString("\n")}\n}"
  }

  final case object GeneralType extends ImpLanType {
    override def toString: String = "GeneralType"
  }

  final case object LongType extends ImpLanType {
    override def toString: String = "Long"
  }

  final case object DoubleType extends ImpLanType {
    override def toString: String = "Double"
  }

  /* Expressions */

  final case object BoolType extends ImpLanType {
    override def toString: String = "Bool"
  }

  final case object UnitType extends ImpLanType {
    override def toString: String = "Unit"
  }

  final case object StringType extends ImpLanType {
    override def toString: String = "String"
  }

  final case object ErrorType extends ImpLanType {
    override def toString: String = "Error"
  }

  final case object UnitValue extends ImpLanVal {
    override def toString: String = "()"
  }

  final case object GeneralValue extends ImpLanVal {
    override def toString: String = "GeneralValue"
  }

  final case object NoError extends ImpLanVal {
    override def toString: String = "NoError"
  }

}
