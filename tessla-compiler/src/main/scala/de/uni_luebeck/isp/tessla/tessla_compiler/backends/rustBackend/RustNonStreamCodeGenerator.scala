/*
 * Copyright 2022 The TeSSLa Community
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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core.{FunctionType => _, _}
import de.uni_luebeck.isp.tessla.core.TesslaAST.{Core, LazyEvaluation, StrictEvaluation}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils.{
  structComparison,
  FinalDeclaration,
  FinalLazyDeclaration,
  VariableDeclaration
}
import de.uni_luebeck.isp.tessla.tessla_compiler._
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.RustUtils.{canBeHashed, convertType}

/**
 * Class handling the translation of non-stream expressions
 */
class RustNonStreamCodeGenerator(extSpec: ExtendedSpecification)
    extends NonStreamCodeGeneratorInterface[String, String](extSpec) {

  /**
   * Translates a function application to Rust
   * @param e The function expression which is applied
   * @param args The argument expressions of the application
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated function application
   */
  override def translateFunctionCall(
    e: ExpressionArg,
    args: Seq[String],
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): String = {
    e match {
      case TypeApplicationExpression(app, types, _) =>
        translateFunctionCall(app, args, tm.typeApp(types), defContext)
      case ExternExpression(name, typ: Core.FunctionType, _) =>
        translateBuiltinFunctionCall(s"__${name}__", args, typ, tm.parsKnown(typ.typeParams))
      case e: ExternExpression => translateExtern(e, tm, defContext)
      case _: FunctionExpression | _: ExpressionRef | _: ApplicationExpression | _: RecordAccessorExpression =>
        s"${translateExpressionArg(e, tm, defContext)}(${args.mkString(", ")})"
      case e =>
        throw Diagnostics.CoreASTError("Function call to expression of wrong type cannot be translated", e.location)
    }
  }

  /**
   * Translates an assignment from TeSSLa Core into Rust
   * @param id The id which is assigned
   * @param e The expression assigned to id
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated assignment
   */
  override def translateAssignment(
    id: Identifier,
    e: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): String = {
    val lazyVar = extSpec.lazyVars.get.contains(id)
    if (lazyVar) {
      definedIdentifiers += (id -> FinalLazyDeclaration)
      val inlinedExp = inlineVars(e, defContext)
      // TODO how exactly do we handle this
      s"let /*lazy*/ var_$id = ${translateExpressionArg(inlinedExp, tm, defContext)};"
    } else if (finalAssignmentPossible(e)) {
      definedIdentifiers += (id -> FinalDeclaration)
      s"let var_$id = ${translateExpressionArg(e, tm, defContext)};"
    } else {
      definedIdentifiers += (id -> VariableDeclaration)
      s"let var_$id = ${translateExpressionArg(e, tm, defContext)};"
    }
  }

  /**
   * Translates a static assignment definition from TeSSLa Core into a static Rust variable
   * @param id The id which is assigned
   * @param e The expression assigned to id
   * @param srcSegments The expression added in three parts to the sections stateStatic, stateDef and stateInit
   */
  def translateStaticAssignment(
    id: Identifier,
    e: ExpressionArg,
    srcSegments: SourceSegments
  ): Unit = {
    val typ = RustUtils.convertType(e.tpe)
    val lazyVar = extSpec.lazyVars.get.contains(id)
    if (lazyVar) {
      definedIdentifiers += (id -> FinalLazyDeclaration)
      val inlinedExp = inlineVars(e, extSpec.spec.definitions)
      // TODO how exactly do we handle this
      srcSegments.stateStatic.append(
        s"let var_$id /* lazy */ = ${translateExpressionArg(inlinedExp, TypeArgManagement.empty, extSpec.spec.definitions)};"
      )
    } else if (finalAssignmentPossible(e)) {
      definedIdentifiers += (id -> FinalDeclaration)
      srcSegments.stateStatic.append(
        s"let var_$id = ${translateExpressionArg(e, TypeArgManagement.empty, extSpec.spec.definitions)};"
      )
    } else {
      definedIdentifiers += (id -> VariableDeclaration)
      srcSegments.stateStatic.append(
        s"let var_$id = ${translateExpressionArg(e, TypeArgManagement.empty, extSpec.spec.definitions)};"
      )
    }
    srcSegments.stateDef.append(s"var_$id: $typ")
    srcSegments.stateInit.append(s"var_$id: var_$id.clone()")
  }

  /**
   * Translates a global function definition from TeSSLa Core into a static Rust function
   * @param id The id which is assigned
   * @param definition The function definition
   * @param srcSegments The translated function is defined globally, and a pointer to it is stored in the state as a TesslaValue
   */
  def translateStaticFunction(
    id: Identifier,
    definition: DefinitionExpression,
    srcSegments: SourceSegments
  ): Unit = {
    definition match {
      case FunctionExpression(_, params, body, result, _) =>
        val genericTypes = RustUtils.getGenericTypeNames(params.map { case (_, _, typ) => typ } :+ result.tpe)
        val traitBounds =
          if (genericTypes.isEmpty) ""
          else s"<${genericTypes.map(t => s"$t: 'static + Clone").mkString(", ")}>"

        val functionParams = params
          .map { case (id, _, tpe) => s"var_$id: ${RustUtils.convertType(tpe)}" }
          .mkString(", ")
        val returnType = RustUtils.convertType(result.tpe)

        srcSegments.static.append(s"fn var_$id$traitBounds($functionParams) -> $returnType {")
        srcSegments.static.appendAll(translateBody(body, result, TypeArgManagement.empty, extSpec.spec.definitions))
        srcSegments.static.append("}")

      /*
        val functionType = RustUtils.convertType(definition.tpe)
        srcSegments.stateStatic.append(s"let var_$id = Value(fn_$id);")
        srcSegments.stateDef.append(s"var_$id: $functionType")
        srcSegments.stateInit.append(s"var_$id: var_$id.clone()")
       */
      case e =>
        throw Diagnostics.CoreASTError("Failed to translate expression as static function", e.location)
    }
  }

  /**
   * Translates a TeSSLa Core FunctionExpression to a Rust expression
   * @param e The function to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated function expression
   */
  override def translateFunction(
    e: FunctionExpression,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): String = {
    definedIdentifiers ++= e.params.map(_._1 -> FinalDeclaration)
    val newTm = tm.parsKnown(e.typeParams)
    val arguments = e.params
      .map {
        case (id, e, tpe) => (id, e, RustUtils.convertType(tpe.resolve(newTm.resMap)))
      }
      .map {
        case (id, StrictEvaluation, tpe) => if (id.fullName == "_") "_" else s"var_$id: $tpe"
        case (id, LazyEvaluation, tpe) =>
          if (id.fullName == "_") "_" else s"var_$id /* lazy */: $tpe" // TODO how to handle lazy...
      }
      .mkString(", ")
    s"|$arguments| {\n" +
      s"${translateBody(e.body, e.result, newTm, defContext).mkString("\n")}" +
      s"\n}"
  }

  /**
   * Translates a block of statements with return expression (i.e. the body of a lambda) to Rust statements
   * @param body The sequence of statements to be translated
   * @param ret The return expression of the block
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated block.
   */
  override def translateBody(
    body: Map[Identifier, DefinitionExpression],
    ret: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): Seq[String] = {
    val newDefContext = defContext ++ body

    val translatedBody = DefinitionOrdering.order(body) collect {
      case (id, exp) if !extSpec.inlining.get.contains(id) => translateAssignment(id, exp, tm, newDefContext)
    }
    translatedBody :+ s"return ${translateExpressionArg(ret, tm, newDefContext)};"
  }

  /**
   * Translates an ExternExpression. If the extern is of function type a lambda expression is wrapped around it.
   * If the extern is directly applied this lambda is most likely unnecessary and this function should not be
   * used for translation of the called extern.
   * @param e          The[[ExternExpression]]
   * @param tm         The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated expression
   */
  override def translateExtern(
    e: ExternExpression,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): String = {
    e match {
      case ExternExpression("true", _, _) =>
        "Value(true)"
      case ExternExpression("false", _, _) =>
        "Value(false)"
      case ExternExpression("None", InstantiatedType("Option", _, _), _) =>
        "Value(None)"
      case ExternExpression(_, typ: Core.FunctionType, _) =>
        val newTm = tm.parsKnown(typ.typeParams)
        val argNames = typ.paramTypes.indices.map(i => s"tLPar_$i")
        val ret = translateFunctionCall(e, argNames, newTm, defContext)
        s"|${argNames.mkString(", ")}|{ return $ret }"
      case ExternExpression(name, _, location) =>
        throw Diagnostics.CoreASTError(s"""Invalid extern(\"$name\") expression could not be translated""", location)
    }
  }

  /**
   * Translates an ExpressionArg to a corresponding Rust expression
   * @param e The expression to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated expression
   */
  override def translateExpressionArg(
    e: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): String = {
    e match {
      case TypeApplicationExpression(e, tArgs, _) =>
        translateExpressionArg(e, tm.typeApp(tArgs), defContext)
      case f: FunctionExpression =>
        translateFunction(f, tm, defContext)
      case ApplicationExpression(
            TypeApplicationExpression(ExternExpression("error", _: Core.FunctionType, _), _, _),
            Seq(message),
            _
          ) =>
        s"Error($message)"
      case ApplicationExpression(e, args, _) =>
        translateFunctionCall(
          e,
          getInlinedArgs(args, defContext).map(translateExpressionArg(_, tm, defContext)),
          tm,
          defContext
        )
      case StringLiteralExpression(value, _) =>
        s"""Value(\"${value.replace("\"", "\\\"")}\".to_string())"""
      case IntLiteralExpression(value, _) =>
        s"Value(${value.toLong}_i64)"
      case FloatLiteralExpression(value, _) =>
        s"Value(${value}_f64)"
      case ExpressionRef(id, _, _) if extSpec.spec.definitions.get(id).exists(_.isInstanceOf[FunctionExpression]) =>
        s"var_${id.fullName}"
      case ExpressionRef(id, _, _) =>
        s"var_${id.fullName}.clone()"
      case x: ExternExpression =>
        translateExtern(x, tm, defContext)
      case RecordConstructorExpression(entries, _) if entries.isEmpty =>
        "Value(())" // Unit value
      case RecordConstructorExpression(entries, _) =>
        if (RustUtils.isStructTuple(entries.toSeq.map { case (name, _) => name }))
          s"Value((${entries.toSeq
            .sortWith { case ((name1, _), (name2, _)) => structComparison(name1, name2) }
            .map { case (_, (exprArg, _)) => translateExpressionArg(exprArg, tm, defContext) }
            .mkString(", ")}))"
        else {
          val resolvedEntries = entries.map { case (name, (ea, loc)) => (name, (ea.tpe.resolve(tm.resMap), loc)) }
          val structName = RustUtils.getStructName(resolvedEntries)

          s"Value($structName { ${entries
            .map { case (name, (ea, _)) => s"$name: ${translateExpressionArg(ea, tm, defContext)}" }
            .mkString(", ")} })"
        }
      case RecordAccessorExpression(name, target, _, _) =>
        val recordNames = target.tpe.asInstanceOf[RecordType].entries.toSeq.map { case (name, _) => name }
        if (RustUtils.isStructTuple(recordNames)) {
          val index = name.stripPrefix("_").toInt
          if (0 < index && index <= recordNames.length)
            s"match ${translateExpressionArg(target, tm, defContext)} { Value(value) => value.${index - 1}, Error(error) => Error(error) }"
          else
            """Error("Invalid tuple index")"""
        } else
          s"match ${translateExpressionArg(target, tm, defContext)} { Value(value) => value.$name, Error(error) => Error(error) }"

      case _ =>
        throw Diagnostics.CoreASTError("Unexpected ExpressionArg cannot be translated", e.location)
    }
  }

  /**
   * Generate all the necessary code and implement any Traits needed for a Rust struct
   * @param structName A reproducible name generated from the struct field names
   * @param fields An ordered list of all fields with their name and type
   * @param srcSegments The struct definition and its accompanying impls are put in the static segment
   */
  def translateStructDefinition(structName: String, fields: Seq[(String, Type)], srcSegments: SourceSegments): Unit = {
    val genericTypes = RustUtils.getGenericTypeNames(fields.map { case (_, typ) => typ })
    val traitBounds = (bound: String) => s"<${genericTypes.map(t => s"$t: $bound").mkString(", ")}>"
    val typeAnnotation = s"<${genericTypes.mkString(", ")}>"
    if (fields.forall { case (_, tpe) => canBeHashed(tpe) })
      srcSegments.static.append("#[derive(std::hash::Hash)]")
    srcSegments.static.append(s"""
       |pub struct $structName$typeAnnotation {
       |    ${fields.map { case (name, tpe) => s"$name: ${convertType(tpe)}" }.mkString(",\n")}
       |}""".stripMargin)
    srcSegments.static.append(s"""
       |impl${traitBounds("Clone")} Clone for $structName$typeAnnotation {
       |    fn clone(&self) -> Self {
       |        $structName {
       |${fields.map { case (name, _) => s"$name: self.$name.clone()" }.mkString(",\n")}
       |        }
       |    }
       |}""".stripMargin)
    srcSegments.static.append(s"""
       |impl${traitBounds("PartialEq + Clone")} Eq for $structName$typeAnnotation {}
       |impl${traitBounds("PartialEq + Clone")} PartialEq for $structName$typeAnnotation {
       |    fn eq(&self, other: &Self) -> bool {
       |${fields.map { case (name, _) => s"PartialEq::eq(&self.$name, &other.$name)" }.mkString("\n&& ")}
       |    }
       |}
       |""".stripMargin)
    val fieldNames = fields.map { case (name, _) => name }
    val isTuple = RustUtils.isStructTuple(fieldNames)
    val wrappedTypeAnnotation = s"<${genericTypes.map(t => s"TesslaValue<$t>").mkString(", ")}>"
    if (isTuple) {
      srcSegments.static.append(generateTupleDisplay(structName, fieldNames, traitBounds, wrappedTypeAnnotation))
    } else {
      srcSegments.static.append(generateStructDisplay(structName, fieldNames, traitBounds, wrappedTypeAnnotation))
    }
    srcSegments.static.append(generateStructParser(structName, fieldNames, isTuple, traitBounds, wrappedTypeAnnotation))
  }

  private def generateTupleDisplay(
    structName: String,
    fieldNames: Seq[String],
    traitBounds: String => String,
    typeAnnotation: String
  ): String = {
    s"""impl${traitBounds("TesslaDisplay + Clone")} TesslaDisplay for $structName$typeAnnotation {
       |    fn tessla_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       |        f.write_str("(")?;
       |${fieldNames
      .map { name => s"""write!(f, \"{}\", self.$name)?;""" }
      .mkString("\nf.write_str(\", \")?;\n")}
       |        f.write_str(")")
       |    }
       |}
       |""".stripMargin
  }

  private def generateStructDisplay(
    structName: String,
    fieldNames: Seq[String],
    traitBounds: String => String,
    typeAnnotation: String
  ): String = {
    s"""impl${traitBounds("TesslaDisplay + Clone")} TesslaDisplay for $structName$typeAnnotation {
       |    fn tessla_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       |        f.write_str("{")?;
       |${fieldNames
      .map { name => s"""write!(f, \"$name = {}\", self.$name)?;""" }
      .mkString("\nf.write_str(\", \")?;\n")}
       |        f.write_str("}")
       |    }
       |}
       |""".stripMargin
  }

  private def generateStructParser(
    structName: String,
    fieldNames: Seq[String],
    isTuple: Boolean,
    traitBounds: String => String,
    typeAnnotation: String
  ): String = {
    val init = s"""let mut result = $structName {\n${fieldNames
      .map { name => s"""$name: Error("Value not assigned while parsing")""" }
      .mkString(",\n")}};"""

    s"""impl${traitBounds("TesslaParse + Clone")} TesslaRecordParse for $structName$typeAnnotation {
       |    fn tessla_parse_struct(s: &str) -> (Result<Self, &'static str>, &str) {
       |        let mut inner = s.trim_start();
       |        $init
       |        while !inner.starts_with("}") {
       |            match inner.split_once(":").or_else(|| inner.split_once("=")) {
       |                Some((lhs, rhs)) => {
       |                    inner = rhs.trim_start();
       |                    match lhs.trim() {
       |${fieldNames
      .map { name => s""""$name" => if !parse_struct_inner(&mut result.$name, &mut inner) { break }""" }
      .mkString(",\n")},
       |                        _ => return (Err("Encountered invalid key while parsing Struct"), inner)
       |                    }
       |                }
       |                None => break
       |            }
       |        }
       |        (Ok(result), inner)
       |    }${if (!isTuple) ""
    else {
      s"""
         |    fn tessla_parse_tuple(s: &str) -> (Result<Self, &'static str>, &str) {
         |        let mut inner = s.trim_start();
         |        let mut i = 1_i64;
         |        $init
         |        while !inner.starts_with(")") {
         |            match i {
         |${fieldNames
           .map { name =>
             s"""${name.substring(1)} => if !parse_struct_inner(&mut result.$name, &mut inner) { break }"""
           }
           .mkString(",\n")},
         |                _ => return (Err("Tuple index out of bounds while parsing"), inner)
         |            }
         |            i += 1;
         |        }
         |        (Ok(result), inner)
         |    }""".stripMargin
    }}
       |}
       |""".stripMargin
  }

  /**
   * Performs the translation of calls to built-in function to a Rust expression
   * Note: Mutable datastructures are not yet handled on this branch since they are not generated
   * @param name Name of the function which is called
   * @param oArgs Argument expressions of the function call
   * @param typeHint The type signature of the called function.
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @return The translated function call in Rust
   */
  def translateBuiltinFunctionCall(
    name: String,
    oArgs: Seq[String],
    typeHint: Core.FunctionType,
    tm: TypeArgManagement
  ): String = {
    val args = oArgs.toIndexedSeq
    name match {

      case "__[rust]box__" => s"TesslaValue::wrap(move ${args(0)})"
      case "__[rust]format__" =>
        s"match ${args(1)} { " +
          s"Value(val) => Value(format!(${args(0).substring(6, args(0).length - 13)}, val)), " +
          s"Error(err) => Error(err) " +
          s"}"

      case "__ite__" | "__staticite__" =>
        s"""match /* if */ (${args(0)}) {
           |    Error(error) => Error(error),
           |    Value(true) /* then */ => { ${args(1)} },
           |    Value(false) /* else */ => { ${args(2)} }
           |}""".stripMargin
      case "__and__" if args.length == 2 =>
        s"""match (${args(0)}) {
           |    Value(true) /* and */ => { ${args(1)} },
           |    false_or_error => false_or_error
           |}""".stripMargin
      case "__and__" =>
        s"""match (${args(0)}) {
           |    Value(true) /* and */ => { ${translateBuiltinFunctionCall(name, args.tail, typeHint, tm)} },
           |    false_or_error => false_or_error
           |}""".stripMargin
      case "__or__" if args.length == 2 =>
        s"""match (${args(0)}) {
           |    Value(false) /* or */ => { ${args(1)} },
           |    true_or_error => true_or_error
           |}""".stripMargin
      case "__or__" =>
        s"""match (${args(0)}) {
           |    Value(false) /* or */ => { ${translateBuiltinFunctionCall(name, args.tail, typeHint, tm)} },
           |    true_or_error => true_or_error
           |}""".stripMargin
      case "__not__" | "__bitflip__"    => s"!(${args(0)})"
      case "__negate__" | "__fnegate__" => s"-${args(0)}"
      case "__eq__"                     => s"${args(0)}.eq(&${args(1)})"
      case "__neq__"                    => s"${args(0)}.ne(&${args(1)})"
      case "__gt__" | "__fgt__"         => s"${args(0)}.gt(&${args(1)})"
      case "__lt__" | "__flt__"         => s"${args(0)}.lt(&${args(1)})"
      case "__geq__" | "__fgeq__"       => s"${args(0)}.ge(&${args(1)})"
      case "__leq__" | "__fleq__"       => s"${args(0)}.le(&${args(1)})"
      case "__add__" | "__fadd__"       => s"(${args(0)} + ${args(1)})"
      case "__String_concat__"          => s"${args(0)}.concat(&${args(1)})"
      case "__sub__" | "__fsub__"       => s"(${args(0)} - ${args(1)})"
      case "__mul__" | "__fmul__"       => s"(${args(0)} * ${args(1)})"
      case "__div__" | "__fdiv__"       => s"(${args(0)} / ${args(1)})"
      case "__mod__"                    => s"(${args(0)} % ${args(1)})"
      case "__bitand__"                 => s"(${args.mkString(" & ")})"
      case "__bitor__"                  => s"(${args.mkString(" | ")})"
      case "__bitxor__"                 => s"(${args.mkString(" ^ ")})"
      case "__leftshift__"              => s"(${args(0)} << ${args(1)})"
      case "__rightshift__"             => s"(${args(0)} >> ${args(1)})"
      case "__abs__" | "__fabs__"       => s"${args(0)}.abs()"

      case "__pow__"  => s"${args(0)}.powf(${args(1)})"
      case "__log__"  => s"${args(0)}.log(${args(1)})"
      case "__sin__"  => s"${args(0)}.sin()"
      case "__cos__"  => s"${args(0)}.cos()"
      case "__tan__"  => s"${args(0)}.tan()"
      case "__atan__" => s"${args(0)}.atan()"

      case "__intToFloat__" => s"TesslaFloat::from(${args(0)})"
      case "__floatToInt__" => s"TesslaInt::from(${args(0)})"

      case "__Some__"    => s"Value(Some(${args(0)}))"
      case "__None__"    => s"Value(None)"
      case "__getSome__" => s"${args(0)}.get_some()"
      case "__isSome__"  => s"${args(0)}.is_some()"
      case "__isNone__"  => s"${args(0)}.is_none()"

      case "__toString__"       => s"${args(0)}.to_string()"
      case "__String_toUpper__" => s"${args(0)}.to_upper()"
      case "__String_toLower__" => s"${args(1)}.to_lower()"

      case "__Map_empty__" => "TesslaMap::empty()"
      //case "__Map_add__" if typeHint.retType.isInstanceOf[MutableMapType] => s"${args(0)}.insert(${args(1)}, ${args(2)})"
      case "__Map_add__"      => s"${args(0)}.add(${args(1)},${args(2)})"
      case "__Map_contains__" => s"${args(0)}.contains(${args(1)})"
      case "__Map_get__"      => s"${args(0)}.get(${args(1)})"
      case "__Map_remove__"   => s"${args(0)}.remove(${args(1)})"
      case "__Map_size__"     => s"${args(0)}.size()"
      case "__Map_fold__"     => s"${args(0)}.fold(${args(1)},${args(2)})"
      case "__Map_keys__"     => s"${args(0)}.keys()"
      case "__Map_map__"      => s"${args(0)}.map(${args(1)})"

      case "__Set_empty__"        => s"TesslaSet::empty()"
      case "__Set_add__"          => s"${args(0)}.add(${args(1)})"
      case "__Set_contains__"     => s"${args(0)}.contains(${args(1)})"
      case "__Set_remove__"       => s"${args(0)}.remove(${args(1)})"
      case "__Set_size__"         => s"${args(0)}.size()"
      case "__Set_union__"        => s"${args(0)}.union(${args(1)})"
      case "__Set_intersection__" => s"${args(0)}.intersection(${args(1)})"
      case "__Set_minus__"        => s"${args(0)}.difference(${args(1)})"
      case "__Set_fold__"         => s"${args(0)}.fold(${args(1)},${args(2)})"
      case "__Set_map__"          => s"${args(0)}.map(${args(1)})"

      case "__List_empty__"   => s"TesslaList::empty()"
      case "__List_size__"    => s"${args(0)}.size()"
      case "__List_append__"  => s"${args(0)}.append(${args(1)})"
      case "__List_prepend__" => s"${args(1)}.prepend(${args(0)})"
      case "__List_tail__"    => s"${args(0)}.tail()"
      case "__List_init__"    => s"${args(0)}.init()"
      case "__List_get__"     => s"${args(0)}.get(${args(1)})"
      case "__List_map__"     => s"${args(0)}.map(${args(1)})"
      //case "__List_set__" if typeHint.retType.isInstanceOf[MutableListType] => s"${args(0)}.insert(${args(1)} as usize, ${args(2)})"
      case "__List_set__"  => s"${args(0)}.set(${args(1)}, ${args(2)})"
      case "__List_fold__" => s"${args(0)}.fold(${args(1)},${args(2)})"

      case s if s.startsWith("__native:") => s"${s.stripPrefix("__native:").stripSuffix("__")}(${args.mkString(", ")})"

      case _ => throw Diagnostics.CommandNotSupportedError(s"Unsupported built-in function for Rust backend: $name")
    }
  }
}
