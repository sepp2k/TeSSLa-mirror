/*
 * Copyright 2021 The TeSSLa Community
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

import de.uni_luebeck.isp.tessla.core.TesslaAST.{Core, LazyEvaluation, StrictEvaluation}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core.{FunctionType => _, _}
import de.uni_luebeck.isp.tessla.tessla_compiler.{
  DefinitionOrdering,
  Diagnostics,
  ExtendedSpecification,
  NonStreamCodeGeneratorInterface
}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils.{
  FinalDeclaration,
  FinalLazyDeclaration,
  VariableDeclaration
}
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.RustUtils.convertType

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
        translateBuiltinFunctionCall(s"__${name}__", args, typ)
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
      s"let mut var_$id = ${translateExpressionArg(e, tm, defContext)};"
    }
  }

  /**
   * Translates a global function definition from TeSSLa Core into a static Rust function
   * @param id The id which is assigned
   * @param definition The function definition
   * @return The translated function
   */
  def translateStaticFunction(
    id: Identifier,
    definition: DefinitionExpression
  ): Seq[String] = {
    definition match {
      case FunctionExpression(_, params, body, result, _) =>
        val genericTypeNames = RustUtils.getGenericTypeNames(params.map { case (_, _, typ) => typ })
        val typeParams = if (genericTypeNames.nonEmpty) s"<${genericTypeNames.mkString(", ")}>" else ""
        val functionParams = params.map { case (id, _, tpe) => s"var_$id: ${RustUtils.convertType(tpe)}" }
        val returnType = RustUtils.convertType(result.tpe)
        (s"fn var_$id$typeParams(${functionParams.mkString(", ")}) -> $returnType {"
          +: translateBody(body, result, TypeArgManagement.empty, extSpec.spec.definitions)
          :+ "}")
      case e =>
        throw Diagnostics.CoreASTError("Non valid function expression cannot be translated", e.location)
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
        case (id, StrictEvaluation, _) => if (id.fullName == "_") "_" else s"var_$id"
        case (id, LazyEvaluation, _) =>
          if (id.fullName == "_") "_" else s"var_$id /* lazy */" // TODO how to handle lazy...
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
    }
  }

  /**
   * Translates an ExpressionArg to a corresponding Rust expression
   * @param e The expression to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
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
      case ApplicationExpression(e, args, _) =>
        translateFunctionCall(
          e,
          getInlinedArgs(args, defContext).map(translateExpressionArg(_, tm, defContext)),
          tm,
          defContext
        )
      case StringLiteralExpression(value, _) =>
        // FIXME: it would be nice to standardise this functionality somewhere...
        s"""Value(\"${value.replace("\"", "\\\"").replace("$", "\\$")}\")"""
      case IntLiteralExpression(value, _) =>
        s"Value(${value.toLong}_i64)"
      case FloatLiteralExpression(value, _) =>
        s"Value(${value}_f64)"
      case ExpressionRef(id, Core.FunctionType(_, _, _, _), _) =>
        s"var_${id.fullName}"
      case ExpressionRef(id, _, _) =>
        s"var_${id.fullName}.clone()" // TODO clone or pass by ref????
      case x: ExternExpression =>
        translateExtern(x, tm, defContext)
      case RecordConstructorExpression(entries, _) if entries.isEmpty =>
        "Value(())" // Unit value
      case RecordConstructorExpression(entries, _) =>
        val fields = entries.toSeq.map { case (name, (ea, _)) => (name, ea.tpe.resolve(tm.resMap)) }
        val structName = RustUtils.getStructName(fields)

        s"Value($structName { ${entries
          .map { case (name, (ea, _)) => s"$name: ${translateExpressionArg(ea, tm, defContext)}" }
          .mkString(", ")} })"
      case RecordAccessorExpression(name, target, _, _) =>
        s"match ${translateExpressionArg(target, tm, defContext)} { Value(value) => value.$name, Error(error) => Error(error) }"
      case _ =>
        throw Diagnostics.CoreASTError("Unexpected ExpressionArg cannot be translated", e.location)
    }
  }

  /**
   * Translates all encountered structs to rust definitions
   * @return The struct definitions
   */
  def translateStructDefinitions(): Seq[String] = {
    // TODO we need some sort of struct from string code generation, if we provide an io interface
    //  we probably also want comparison
    RustUtils.definedStructs.toSeq.map {
      case (structName, fields) =>
        s"""struct $structName {
           |${fields.map { case (name, tpe) => s"$name: ${convertType(tpe)}" }.mkString(",\n")}
           |}
           |impl Clone for $structName {
           |    fn clone(&self) -> Self {
           |        $structName {
           |${fields.map { case (name, _) => s"$name: self.$name.clone()" }.mkString(",\n")}
           |        }
           |    }
           |}""".stripMargin
    }
  }

  /**
   * Performs the translation of calls to built-in function to a Rust expression
   * Note: Mutable datastructures are not yet handled on this branch since they are not generated
   * @param name Name of the function which is called
   * @param oArgs Argument expressions of the function call
   * @param typeHint The type signature of the called function.
   * @return The translated function call in Rust
   */
  def translateBuiltinFunctionCall(
    name: String,
    oArgs: Seq[String],
    typeHint: Core.FunctionType
  ): String = {
    val args = oArgs.toIndexedSeq
    name match {
      case "__[TC]inputParse__" => "" // TODO RustIOHandling.getInputParseExpression(typeHint.retType, args(0))
      // TODO some brackets here are superfluous and will produce warnings
      case "__ite__" | "__staticite__" =>
        s"match ${args(0)} { Value(true) => { ${args(1)} }, Value(false) => { ${args(2)} }, Error(error) => Error(error) }"
      case "__and__" if args.length == 2 =>
        s"match ${args(0)} { Value(true) => { ${args(1)} }, false_or_error => false_or_error }"
      case "__and__" =>
        s"match ${args(0)} { Value(true) => { ${translateBuiltinFunctionCall(name, args.tail, typeHint)} }, false_or_error => false_or_error }"
      case "__or__" if args.length == 2 =>
        s"match ${args(0)} { Value(false) => { ${args(1)} }, true_or_error => true_or_error }"
      case "__or__" =>
        s"match ${args(0)} { Value(false) => { ${translateBuiltinFunctionCall(name, args.tail, typeHint)} }, true_or_error => true_or_error }"
      case "__not__" | "__bitflip__"    => s"!(${args(0)})"
      case "__negate__" | "__fnegate__" => s"-${args(0)}"
      case "__eq__"                     => s"${args(0)}.eq(&${args(1)})"
      case "__neq__"                    => s"${args(0)}.ne(&${args(1)})"
      case "__gt__" | "__fgt__"         => s"${args(0)}.gt(&${args(1)})"
      case "__lt__" | "__flt__"         => s"${args(0)}.lt(&${args(1)})"
      case "__geq__" | "__fgeq__"       => s"${args(0)}.ge(&${args(1)})"
      case "__leq__" | "__fleq__"       => s"${args(0)}.le(&${args(1)})"
      case "__add__" | "__fadd__"       => s"(${args(0)} + ${args(1)})"
      case "__String_concat__"          => s"(${args(0)} + ${args(1)}.as_str())"
      case "__sub__" | "__fsub__"       => s"(${args(0)} - ${args(1)})"
      case "__mul__" | "__fmul__"       => s"(${args(0)} * ${args(1)})"
      case "__div__" | "__fdiv__"       => s"(${args(0)} / ${args(1)})"
      case "__mod__"                    => s"(${args(0)} % ${args(1)})"
      case "__bitand__"                 => s"(${args.mkString(" & ")})"
      case "__bitor__"                  => s"(${args.mkString(" | ")})"
      case "__bitxor__"                 => s"(${args.mkString(" ^ ")})"
      case "__leftshift__"              => s"(${args(0)} << ${args(1)})"
      case "__rightshift__"             => s"(${args(0)} >> ${args(1)})"

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

      case "__toString__"      => s"${args(0)}.to_string()"
      case "__String_format__" => s"${args(0)}.format(${args(1)})"

      /* TODO https://docs.rs/im/15.0.0/im/
      case "__Map_empty__" => "im::HashMap::new()"
      case "__Map_add__" if typeHint.retType.isInstanceOf[MutableMapType] =>
        s"${args(0)}.insert(${args(1)}, ${args(2)})"
      case "__Map_add__"      => s"${args(0)} + ((${args(1)}) -> (${args(2)}))"
      case "__Map_contains__" => s"${args(0)}.contains_key(${args(1)})"
      case "__Map_get__"      => s"${args(0)}.get(${args(1)})"
      case "__Map_remove__"   => s"${args(0)}.remove(${args(1)})"
      case "__Map_size__"     => s"${args(0)}.len()"
      case "__Map_fold__" =>
        s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)}){case (c, (k, v)) => val f = ${args(2)}; f(c, k, v)}"
      case "__Map_keys__" => s"Vec::from_iter(${args(0)}.keys())"

      case "__Set_empty__"        => "std::collections::HashSet::new()"
      case "__Set_add__"          => s"${args(0)}.insert(${args(1)})"
      case "__Set_contains__"     => s"${args(0)}.contains(${args(1)})"
      case "__Set_remove__"       => s"${args(0)}.remove(${args(1)})"
      case "__Set_size__"         => s"${args(0)}.len()"
      case "__Set_union__"        => s"${args(0)}.union(${args(1)})"
      case "__Set_intersection__" => s"${args(0)}.intersection(${args(1)})"
      case "__Set_minus__"        => s"${args(0)}.difference(${args(1)})"
      case "__Set_fold__"         => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)})(${args(2)})"

      case "__List_empty__"   => s"std::vec::Vec::new()"
      case "__List_size__"    => s"${args(0)}.len()"
      case "__List_append__"  => s"${args(0)}.append(${args(1)})"
      case "__List_prepend__" => s"${args(0)}.insert(0, ${args(1)})"
      case "__List_tail__"    => s"${args(0)}.split_off(1)"
      case "__List_init__"    => s"${args(0)}.truncate(${args(0)}.len() - 1)"
      case "__List_get__"     => s"${args(0)}[${args(1)} as usize]"
      case "__List_set__" if typeHint.retType.isInstanceOf[MutableListType] =>
        s"${args(0)}.insert(${args(1)} as usize, ${args(2)})"
      case "__List_set__"  => s"${args(0)}.updated(${args(1)}.asInstanceOf[Int], ${args(2)})"
      case "__List_fold__" => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)})(${args(2)})"
       */

      case _ => throw Diagnostics.CommandNotSupportedError(s"Unsupported built-in function for Rust backend: $name")
    }
  }
}
