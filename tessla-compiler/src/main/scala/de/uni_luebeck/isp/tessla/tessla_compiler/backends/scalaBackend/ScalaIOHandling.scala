/*

 */

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.{Diagnostics, IntermediateCodeUtils}

/**
 * Class containing code for parsing values from and to strings. Used for generating the input/output parsing
 * from stdio.
 *
 * NOTE: A general parsing method which can be executed at runtime is not possible for input parsing since tuples
 * of various type cannot be generated. Replacing tuples internally by Seq[Any] would be a solution but
 * isn't nice at all
 */
object ScalaIOHandling {

  /**
   * Produces an expression in Scala which is parsing a value from a string to a concrete value
   * @param to The type to which the input should be parsed
   * @param exp The expression which evaluates to the string that is parsed
   * @param freshVarCount A counter for fresh vars which are used in the parse expression. Must be incremented by 1
   *                      every time a fresh var is introduced
   * @return A scala expression with exp as subexpression returning a value of type to
   */
  def getInputParseExpression(to: ImpLanType, exp: String, freshVarCount: Int = 0): String = {
    to match {
      case LongType   => getInputParseExpressionForLong(exp)
      case DoubleType => getInputParseExpressionForDouble(exp)
      case BoolType   => getInputParseExpressionForBoolean(exp)
      case UnitType   => getInputParseExpressionForUnit(exp)
      case StringType => getInputParseExpressionForString(exp)
      case _ =>
        val s = to match {
          case OptionType(t)             => getInputParseExpressionForOption(t, exp, freshVarCount)
          case ImmutableSetType(valType) => getInputParseExpressionForImmutableSet(valType, exp, freshVarCount)
          case ImmutableMapType(keyType, valType) =>
            getInputParseExpressionForImmutableMap(keyType, valType, exp, freshVarCount)
          case ImmutableListType(valType) => getInputParseExpressionForImmutableList(valType, exp, freshVarCount)
          case StructType(subTypes, fieldNames)
              if IntermediateCodeUtils.structIsTuple(StructType(subTypes, fieldNames)) =>
            getInputParseExpressionForTuple(subTypes, exp, freshVarCount)
          case StructType(subTypes, fieldNames) =>
            getInputParseExpressionForStruct(fieldNames, subTypes, exp, freshVarCount)
          case t =>
            throw Diagnostics.CommandNotSupportedError(
              s"Input parsing of type $t is not supported in the Scala translation"
            )
        }
        s + s".asInstanceOf[${ScalaConstants.typeTranslation(to)}]"
    }
  }

  private def getInputParseExpressionForLong(exp: String): String = {
    "{try {java.lang.Long.parseLong(" + exp + ")} catch {case t : Throwable =>  throw InputError(s\"Long input has invalid format\", t.getMessage)}}"
  }

  private def getInputParseExpressionForDouble(exp: String): String = {
    "{try {java.lang.Double.parseDouble(" + exp + ")} catch {case t : Throwable =>  throw InputError(s\"Double input has invalid format\", t.getMessage)}}"
  }

  private def getInputParseExpressionForBoolean(exp: String): String = {
    "{val b = " + exp + "; if (b == \"true\") {true} else if (b == \"false\") {false} else {throw InputError(s\"Boolean input has invalid format\", b)}}"
  }

  private def getInputParseExpressionForUnit(exp: String): String = {
    "{val u = " + exp + "; if (u == \"()\") true else throw InputError(\"Unit input has invalid format\", u)}"
  }

  private def getInputParseExpressionForString(exp: String): String = {
    s"processStringInput($exp)"
  }

  private def getInputParseExpressionForOption(subType: ImpLanType, exp: String, freshVarCount: Int = 0): String = {
    val expName = s"exp$freshVarCount"
    s"""{
       |val $expName = $exp;
       |try {
       |if ($expName == "None") {
        |EONone()
       |} else if ($expName.startsWith("Some(") && $expName.endsWith(")")) {
        |EOSome(${getInputParseExpression(
      subType,
      s"""$expName.stripPrefix("Some(").stripSuffix(")").strip""",
      freshVarCount + 1
    )})
       |} else {
        |throw new java.lang.Exception()
       |}
       |} catch {
        |case i : InputError => throw i;
        |case _ : Throwable => throw InputError("Option input has invalid format", $expName)
       |}
       |}
     |""".stripMargin.replaceAll("\n", " ")
  }

  private def getInputParseExpressionForImmutableSet(
    subType: ImpLanType,
    exp: String,
    freshVarCount: Int = 0
  ): String = {
    val expName = s"exp$freshVarCount"
    s"""{val out = $exp;
       |try {
       |if (out.startsWith("Set(") && out.endsWith(")")) {
        |val out_str = out.stripPrefix("Set(").stripSuffix(")").strip;
        |if (out_str == "") {
         |Set()
        |} else {
         |splitString(out_str, ",").map{$expName => ${getInputParseExpression(
      subType,
      s"$expName.strip",
      freshVarCount + 1
    )} }.toSet
        |}
        |} else {
         |throw new java.lang.Exception()
        |}
       |} catch {
        |case i : InputError => throw i;
        |case _ : Throwable => throw InputError("Set input has invalid format", out)
       |}
       |}
    |""".stripMargin.replaceAll("\n", " ")
  }

  private def getInputParseExpressionForImmutableMap(
    keyType: ImpLanType,
    valType: ImpLanType,
    exp: String,
    freshVarCount: Int = 0
  ): String = {
    val expName = s"exp$freshVarCount"
    s"""{val out = $exp;
       |try {
       |if (out.startsWith("Map(") && out.endsWith(")")) {
        |val out_str = out.stripPrefix("Map(").stripSuffix(")").strip;
        |if (out_str == "") {
         |Map()
        |} else {
         |splitString(out_str, ",").map{s =>
          |val $expName = splitString(s.strip, "->");
          |if ($expName.size == 2) {
           |((${getInputParseExpression(keyType, s"""$expName(0).strip""", freshVarCount + 1)})
           |->
           |(${getInputParseExpression(valType, s"""$expName(1).strip""", freshVarCount + 1)}))
          |} else {
           |throw new java.lang.Exception()
          |}
        |}.toMap
       |}
       |} else {
         |throw new java.lang.Exception()
       |}
       |} catch {
        |case i : InputError => throw i;
        |case _ : Throwable => throw InputError("Map input has invalid format", out)
       |}
       |}
    |""".stripMargin.replaceAll("\n", " ")
  }

  private def getInputParseExpressionForImmutableList(
    subType: ImpLanType,
    exp: String,
    freshVarCount: Int = 0
  ): String = {
    val expName = s"exp$freshVarCount"
    s"""{val out = $exp;
       |try {
       |if (out.startsWith("List(") && out.endsWith(")")) {
        |val out_str = out.stripPrefix("List(").stripSuffix(")").strip;
        |if (out_str == "") {
         |List()
        |} else {
         |splitString(out_str, ",").map{$expName => ${getInputParseExpression(
      subType,
      s"$expName.strip",
      freshVarCount + 1
    )} }.toList
        |}
        |} else {
         |throw new java.lang.Exception()
        |}
        |} catch {
         |case i : InputError => throw i;
         |case _ : Throwable => throw InputError("List input has invalid format", out)
        |}
        |}
    |""".stripMargin.replaceAll("\n", " ")
  }

  private def getInputParseExpressionForTuple(
    subTypes: Seq[ImpLanType],
    exp: String,
    freshVarCount: Int = 0
  ): String = {
    val expName = s"exp$freshVarCount"
    s"""{
       |val s = $exp;
       |try {
       |if (s.startsWith("(") && s.endsWith(")")) {
        |val $expName = splitString(s.substring(1, s.length - 1).strip, ",");
        |if ($expName.size != ${subTypes.length}) {
         |throw new java.lang.Exception()
        |};
        |(${subTypes.indices
      .map(i => getInputParseExpression(subTypes(i), s"$expName($i).strip", freshVarCount + 1))
      .mkString(", ")})
       |} else {
        |throw new java.lang.Exception()
       |}
       |} catch {
        |case i : InputError => throw i;
        |case _ : Throwable => throw InputError("Tuple input has invalid format", s)
       |}
       |}
    |""".stripMargin.replaceAll("\n", " ")
  }

  private def getInputParseExpressionForStruct(
    fieldNames: Seq[String],
    subTypes: Seq[ImpLanType],
    exp: String,
    freshVarCount: Int = 0
  ): String = {
    val expName = s"exp$freshVarCount"
    s"""{
       |val s = $exp;
       |try {
       |if (s.startsWith("{") && s.endsWith("}")) {
        |val cont = splitString(s.substring(1, s.length - 1).strip, ",").map(splitString(_, "=").map(_.strip));
         |if (cont.size != ${fieldNames.size} || cont.exists(_.size != 2)) {
           |throw new java.lang.Exception()
         |};
        |val $expName = cont.map(s => (s(0), s(1))).toMap;
         |(${subTypes.indices
      .map(i =>
        getInputParseExpression(subTypes(i), s"$expName(" + "\"" + fieldNames(i) + "\").strip", freshVarCount + 1)
      )
      .mkString(", ")})
        |} else {
         |throw new java.lang.Exception()
        |}
       |} catch {
        |case i : InputError => throw i;
        |case _ : Throwable => throw InputError("Struct input has invalid format", s)
       |}
       |}
    |""".stripMargin.replaceAll("\n", " ")
  }

  /**
   * Produces a Scala expression generating a string representation of a value that can be printed to stdout
   *
    * @param from Type of the value from which the string is generated
   * @param exp The expression evaluating to the value which shall be converted
   * @param freshVarCount A counter for fresh vars which are used in the conversion expression. Must be incremented by 1
   *                      every time a fresh var is introduced
   * @return Conversion expression in Scala
   */
  def getParseExpressionToString(from: ImpLanType, exp: String, freshVarCount: Int = 0): String = {
    from match {
      case LongType | DoubleType | BoolType   => s"String.valueOf($exp)"
      case UnitType                           => "\"()\""
      case StringType                         => exp
      case OptionType(t)                      => getParseExpressionToStringForOption(t, exp, freshVarCount)
      case ImmutableSetType(valType)          => getParseExpressionToStringForSet(valType, exp, freshVarCount)
      case ImmutableMapType(keyType, valType) => getParseExpressionToStringForMap(keyType, valType, exp, freshVarCount)
      case ImmutableListType(valType)         => getParseExpressionToStringForList(valType, exp, freshVarCount)
      case StructType(subTypes, fieldNames) if IntermediateCodeUtils.structIsTuple(StructType(subTypes, fieldNames)) =>
        getParseExpressionToStringForTuple(subTypes, exp, freshVarCount)
      case StructType(subTypes, fieldNames) =>
        getParseExpressionToStringForRecord(fieldNames, subTypes, exp, freshVarCount)
      case GeneralType => throw Diagnostics.DSLError(s"General type expression $exp cannot be used for printing")
      case _           => s"$exp.toString()"
    }
  }

  private def getParseExpressionToStringForOption(subType: ImpLanType, exp: String, freshVarCount: Int = 0): String = {
    val expName = s"exp$freshVarCount"
    s"""{val $expName = $exp; if (${expName}.isDefined) "Some(" + ${getParseExpressionToString(
      subType,
      expName + ".get",
      freshVarCount + 1
    )} + ")" else "None"}"""
  }

  private def getParseExpressionToStringForSet(valType: ImpLanType, exp: String, freshVarCount: Int = 0): String = {
    s""""Set(" + ${exp}.toSeq.map{e => ${getParseExpressionToString(
      valType,
      "e",
      freshVarCount + 1
    )}}.mkString(", ") + ")""""
  }

  private def getParseExpressionToStringForMap(
    keyType: ImpLanType,
    valType: ImpLanType,
    exp: String,
    freshVarCount: Int = 0
  ): String = {
    s""""Map(" + ${exp}.map{case (k,v) => ${getParseExpressionToString(keyType, "k", freshVarCount + 1)} + " -> " +
       |${getParseExpressionToString(valType, "v", freshVarCount + 1)} }.mkString(", ") + ")"
    """.stripMargin.replaceAll("\n", " ")
  }

  private def getParseExpressionToStringForList(valType: ImpLanType, exp: String, freshVarCount: Int = 0): String = {
    s""""List(" + ${exp}.map{e => ${getParseExpressionToString(
      valType,
      "e",
      freshVarCount + 1
    )}}.mkString(", ") + ")""""
  }

  private def getParseExpressionToStringForRecord(
    fieldNames: Seq[String],
    subTypes: Seq[ImpLanType],
    exp: String,
    freshVarCount: Int = 0
  ): String = {
    val expName = s"exp$freshVarCount"
    val elems = fieldNames
      .zip(subTypes)
      .zipWithIndex
      .map {
        case ((n, t), i) =>
          val add = if (fieldNames.size == 1) "" else s"._${i + 1}"
          s""""$n = " + ${getParseExpressionToString(t, s"$expName$add", freshVarCount + 1)}"""
      }
      .mkString(""" + ", " + """)
    s"""{val $expName = $exp; "{" + $elems + "}"}"""
  }

  private def getParseExpressionToStringForTuple(
    subTypes: Seq[ImpLanType],
    exp: String,
    freshVarCount: Int = 0
  ): String = {
    val expName = s"exp$freshVarCount"
    val elems = subTypes.zipWithIndex
      .map {
        case (t, i) =>
          s"""${getParseExpressionToString(t, s"$expName._${i + 1}", freshVarCount + 1)}"""
      }
      .mkString(""" + ", " + """)
    s"""{val $expName = $exp; "(" + $elems + ")"}"""
  }

}
