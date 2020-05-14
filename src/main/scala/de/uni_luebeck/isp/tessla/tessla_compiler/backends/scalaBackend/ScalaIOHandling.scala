package de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.{Errors, IntermediateCodeUtils}

/*
 * NOTE: This way of input parsing shows advantages, when complex input parsing (Sets, records ...) is used rarely.
 * We assume that since we think the standard input will be plain numbers
 * Another way of handling the inputs would be a method in the scala skeleton parsing all inputs and returning type
 * Any. In cases of extensive input parsing it would make the generated code shorter and a lot cleaner.
 * However we will stay with this version for now.
 */

object ScalaIOHandling {

  def getInputParseExpression(to: ImpLanType, exp: String, freshVarCount : Int = 0) : String = {
    to match {
      case LongType => getInputParseExpressionForLong(exp)
      case DoubleType => getInputParseExpressionForDouble(exp)
      case BoolType => getInputParseExpressionForBoolean(exp)
      case UnitType => getInputParseExpressionForUnit()
      case StringType => getInputParseExpressionForString(exp)
      case _ =>
        val s = to match {
        case OptionType(t) => getInputParseExpressionForOption(t, exp, freshVarCount)
        case ImmutableSetType(valType) => getInputParseExpressionForImmutableSet(valType, exp, freshVarCount)
        case ImmutableMapType(keyType, valType) => getInputParseExpressionForImmutableMap(keyType, valType, exp, freshVarCount)
        case ImmutableListType(valType) => getInputParseExpressionForImmutableList(valType, exp, freshVarCount)
        case StructType(subTypes, fieldNames) if IntermediateCodeUtils.structIsTuple(StructType(subTypes, fieldNames)) => getInputParseExpressionForTuple(subTypes, exp, freshVarCount)
        case StructType(subTypes, fieldNames) => getInputParseExpressionForStruct(fieldNames, subTypes, exp, freshVarCount)
        case t => throw Errors.CommandNotSupportedError(s"Input parsing of type $t is not supported in the Scala translation")
        }
        s + s".asInstanceOf[${ScalaConstants.typeTranslation(to)}]"
    }
  }

  def getInputParseExpressionForLong(exp: String) : String = {
    s"java.lang.Long.parseLong($exp)"
  }

  def getInputParseExpressionForDouble(exp: String) : String = {
    s"java.lang.Double.parseDouble($exp)"
  }

  def getInputParseExpressionForBoolean(exp: String) : String = {
    s"java.lang.Boolean.parseBoolean($exp)"
  }

  def getInputParseExpressionForUnit() : String = {
    "true"
  }

  def getInputParseExpressionForString(exp: String) : String = {
    s"processStringInput($exp)"
  }

  def getInputParseExpressionForOption(subType: ImpLanType, exp: String, freshVarCount : Int = 0) : String = {
    val expName = s"exp$freshVarCount"
    s"""{val $expName = $exp;
       |if ($expName == "None") {
        |None
       |} else if ($expName.startsWith("Some(") && $expName.endsWith(")")) {
        |Some(${getInputParseExpression(subType, s"""$expName.stripPrefix("Some(").stripSuffix(")").strip""", freshVarCount + 1)})
       |} else {
        |throw new java.lang.Exception("Option input has invalid format")
       |}
       |}
     |""".stripMargin.replaceAll("\n", " ")
  }

  def getInputParseExpressionForImmutableSet(subType: ImpLanType, exp: String, freshVarCount : Int = 0) : String = {
    val expName = s"exp$freshVarCount"
    s"""{val out = $exp;
       |if (out.startsWith("Set(") && out.endsWith(")")) {
        |val out_str = out.stripPrefix("Set(").stripSuffix(")").strip;
        |if (out_str == "") {
         |Set()
        |} else {
         |splitString(out_str, ",").map{$expName => ${getInputParseExpression(subType, s"$expName.strip", freshVarCount + 1)} }.toSet
        |}
        |} else {
         |throw new java.lang.Exception("Set input has invalid format")
        |}
        |}
    |""".stripMargin.replaceAll("\n", " ")
  }

  def getInputParseExpressionForImmutableMap(keyType: ImpLanType, valType: ImpLanType, exp: String, freshVarCount : Int = 0) : String = {
    val expName = s"exp$freshVarCount"
    s"""{val out = $exp;
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
           |throw new java.lang.Exception("Map input has invalid format")
          |}
        |}.toMap
       |}
       |} else {
         |throw new java.lang.Exception("Map input has invalid format")
       |}
       |}
    |""".stripMargin.replaceAll("\n", " ")
  }

  def getInputParseExpressionForImmutableList(subType: ImpLanType, exp: String, freshVarCount : Int = 0) : String = {
    val expName = s"exp$freshVarCount"
    s"""{val out = $exp;
       |if (out.startsWith("List(") && out.endsWith(")")) {
        |val out_str = out.stripPrefix("List(").stripSuffix(")").strip;
        |if (out_str == "") {
         |List()
        |} else {
         |splitString(out_str, ",").map{$expName => ${getInputParseExpression(subType, s"$expName.strip", freshVarCount + 1)} }.toList
        |}
        |} else {
         |throw new java.lang.Exception("List input has invalid format")
        |}
        |}
    |""".stripMargin.replaceAll("\n", " ")
  }

  def getInputParseExpressionForTuple(subTypes: Seq[ImpLanType], exp: String, freshVarCount : Int = 0) : String = {
    val expName = s"exp$freshVarCount"
    s"""{val s = $exp;
       |if (s.startsWith("(") && s.endsWith(")")) {
        |val $expName = splitString(s.substring(1, s.length - 1).strip, ",");
        |if ($expName.size != ${subTypes.length}) {
         |throw new java.lang.Exception("Tuple input has invalid format")
        |};
        |(${subTypes.indices.map(i => getInputParseExpression(subTypes(i), s"$expName($i).strip", freshVarCount + 1)).mkString(", ")})
       |} else {
        |throw new java.lang.Exception("Tuple input has invalid format")
       |}
       |}
    |""".stripMargin.replaceAll("\n", " ")
  }

  def getInputParseExpressionForStruct(fieldNames: Seq[String], subTypes: Seq[ImpLanType], exp: String, freshVarCount : Int = 0) : String = {
    val expName = s"exp$freshVarCount"
    s"""{val s = $exp;
       |if (s.startsWith("{") && s.endsWith("}")) {
        |val cont = splitString(s.substring(1, s.length - 1).strip, ",").map(splitString(_, "=").map(_.strip));
         |if (cont.size != ${fieldNames.size} || cont.exists(_.size != 2)) {
           |throw new java.lang.Exception("Record input has invalid format")
         |};
        |val $expName = cont.map(s => (s(0), s(1))).toMap;
         |(${subTypes.indices.map(i => getInputParseExpression(subTypes(i), s"$expName(" + "\"" + fieldNames(i) + "\").strip", freshVarCount + 1)).mkString(", ")})
        |} else {
         |throw new java.lang.Exception("Record input has invalid format")
        |}
       |}
    |""".stripMargin.replaceAll("\n", " ")
  }

  def getParseExpressionToString(from: ImpLanType, exp: String, freshVarCount : Int = 0) : String = {
    from match {
      case LongType |
           DoubleType |
           BoolType => s"String.valueOf($exp)"
      case UnitType => "\"()\""
      case StringType => exp
      case OptionType(t) => getParseExpressionToStringForOption(t, exp, freshVarCount)
      case ImmutableSetType(valType) => getParseExpressionToStringForSet(valType, exp, freshVarCount)
      case ImmutableMapType(keyType, valType) => getParseExpressionToStringForMap(keyType, valType, exp, freshVarCount)
      case ImmutableListType(valType) => getParseExpressionToStringForList(valType, exp, freshVarCount)
      case StructType(subTypes, fieldNames) if IntermediateCodeUtils.structIsTuple(StructType(subTypes, fieldNames)) =>
        getParseExpressionToStringForTuple(subTypes, exp, freshVarCount)
      case StructType(subTypes, fieldNames) => getParseExpressionToStringForRecord(fieldNames, subTypes, exp, freshVarCount)
      case GeneralType => throw Errors.DSLError(s"General type expression $exp cannot be used for printing")
      case _ => s"$exp.toString()"
    }
  }

  def getParseExpressionToStringForOption(subType: ImpLanType, exp: String, freshVarCount : Int = 0) : String = {
    val expName = s"exp$freshVarCount"
    s"""{val $expName = $exp; if (${expName}.isDefined) "Some(" + ${getParseExpressionToString(subType, expName + ".get", freshVarCount + 1)} + ")" else "None"}"""
  }

  def getParseExpressionToStringForSet(valType: ImpLanType, exp: String, freshVarCount : Int = 0) : String = {
    s""""Set(" + ${exp}.toSeq.map{e => ${getParseExpressionToString(valType, "e", freshVarCount + 1)}}.mkString(", ") + ")""""
  }

  def getParseExpressionToStringForMap(keyType: ImpLanType, valType: ImpLanType, exp: String, freshVarCount : Int = 0) : String = {
    s""""Map(" + ${exp}.map{case (k,v) => ${getParseExpressionToString(keyType, "k", freshVarCount + 1)} + " -> " +
       |${getParseExpressionToString(valType, "v", freshVarCount + 1)} }.mkString(", ") + ")"
    """.stripMargin.replaceAll("\n", " ")
  }

  def getParseExpressionToStringForList(valType: ImpLanType, exp: String, freshVarCount : Int = 0) : String = {
    s""""List(" + ${exp}.map{e => ${getParseExpressionToString(valType, "e", freshVarCount + 1)}}.mkString(", ") + ")""""
  }

  def getParseExpressionToStringForRecord(fieldNames: Seq[String], subTypes: Seq[ImpLanType], exp: String, freshVarCount : Int = 0) : String = {
    val expName = s"exp$freshVarCount"
        val elems = fieldNames.zip(subTypes).zipWithIndex.map { case ((n, t), i) =>
          s""""$n = " + ${getParseExpressionToString(t, s"$expName._${i+1}", freshVarCount + 1)}"""
        }.mkString(""" + ", " + """)
        s"""{val $expName = $exp; "{" + $elems + "}"}"""
  }

  def getParseExpressionToStringForTuple(subTypes: Seq[ImpLanType], exp: String, freshVarCount : Int = 0) : String = {
    val expName = s"exp$freshVarCount"
        val elems = subTypes.zipWithIndex.map { case (t, i) =>
          s"""${getParseExpressionToString(t, s"$expName._${i+1}", freshVarCount + 1)}"""
        }.mkString(""" + ", " + """)
        s"""{val $expName = $exp; "(" + $elems + ")"}"""
  }

}
