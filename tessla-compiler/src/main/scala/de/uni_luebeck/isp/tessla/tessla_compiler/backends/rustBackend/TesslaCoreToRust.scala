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

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Result
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.tessla_compiler.{DefinitionOrdering, Diagnostics, ExtendedSpecification}

import scala.annotation.tailrec
import scala.io.Source

/**
 * Class implementing TranslationPhase for the translation from TeSSLa Core to
 * rust code
 * The translation of stream functions is performed in RustStreamCodeGenerator
 * The translation of other expressions in RustNonStreamCodeGenerator
 * @param ioInterface Indicates whether the generated code shall be able to read/write from/to stdio
 */
class TesslaCoreToRust(ioInterface: Boolean) extends TranslationPhase[ExtendedSpecification, String] {
  private val sourceTemplate: String =
    if (ioInterface) "de/uni_luebeck/isp/tessla/tessla_compiler/RustSkeleton.rs"
    else "de/uni_luebeck/isp/tessla/tessla_compiler/RustSkeletonNoIO.rs"

  override def translate(extSpec: ExtendedSpecification): Result[String] =
    new Translator(extSpec).translate()

  class Translator(extSpec: ExtendedSpecification) extends TranslationPhase.Translator[String] {

    val rustNonStreamCodeGenerator = new RustNonStreamCodeGenerator(extSpec)
    val rustStreamCodeGenerator = new RustStreamCodeGenerator(rustNonStreamCodeGenerator)

    @tailrec
    final protected def externResolution(e: ExpressionArg): ExternExpression = e match {
      case e: ExternExpression => e
      case ExpressionRef(id, _, _) if extSpec.spec.definitions.contains(id) =>
        externResolution(extSpec.spec.definitions(id))
      case _ =>
        throw Diagnostics.CoreASTError(
          "No extern or reference to extern in function application with stream result",
          e.location
        )
    }

    protected def getStreamType(id: Identifier): Type = {
      if (extSpec.spec.definitions.contains(id)) {
        extSpec.spec.definitions(id).tpe
      } else if (extSpec.spec.in.contains(id)) {
        extSpec.spec.in(id)._1
      } else {
        throw Diagnostics.CoreASTError(s"Type of stream $id cannot be found")
      }
    }

    override protected def translateSpec(): String = {
      val srcSegments = SourceSegments()

      // Produce computation section
      DefinitionOrdering.order(extSpec.spec.definitions).foreach {
        case (id, definition) =>
          definition.tpe match {
            case InstantiatedType("Events", _, _) =>
              rustStreamCodeGenerator
                .translateStreamDefinitionExpression(id, definition, extSpec.spec.definitions, srcSegments)
            case FunctionType(_, _, _, _) =>
              definition match {
                case FunctionExpression(_, params, body, result, _) =>
                  srcSegments.static.appendAll(translateStaticFunction(id, params, body, result))
                case e =>
                  throw Diagnostics.CoreASTError("Non valid function expression cannot be translated", e.location)
              }
            case e =>
              throw Diagnostics
                .NotYetImplementedError("Failed to translate an expression without instantiated type", e.location)
          }
      }

      // Produce output generation
      extSpec.spec.out.foreach { o =>
        val name = TesslaAST.Core.getOutputName(o._2)
        rustStreamCodeGenerator.produceOutputCode(
          o._1.id,
          getStreamType(o._1.id),
          name,
          srcSegments,
          o._2.contains("raw")
        )
      }

      // Produce input consumption
      extSpec.spec.in.foreach { i =>
        rustStreamCodeGenerator.produceInputCode(i._1, i._2._1, srcSegments)
      }

      insertSegments(srcSegments)
      // Rust does not like $ in names
        .replace("$", "京")
        .replace("\\京", "$")
    }

    private def insertSegments(srcSegments: SourceSegments): String = {
      val source = Source.fromResource(sourceTemplate).mkString
      val rewrittenSource = source
        .replace("//USERINCLUDES", "") //TODO
        .replace("//STATIC", srcSegments.static.mkString("\n"))
        .replace("//VARIABLES", srcSegments.variables.mkString("\n"))
        .replace("//STORE", srcSegments.store.mkString("\n"))
        .replace("//TIMESTAMP", srcSegments.timestamp.mkString("\n"))
        .replace("//COMPUTATION", srcSegments.computation.mkString("\n"))
        .replace("//OUTPUT", srcSegments.output.mkString("\n"))
        .replace("//INPUT", srcSegments.input.mkString("\n"))
      rewrittenSource
    }

    private def translateStaticFunction(
      id: Identifier,
      params: List[(Identifier, Evaluation, Type)],
      body: Map[Identifier, DefinitionExpression],
      result: ExpressionArg
    ): Seq[String] = {
      val genericTypeNames = RustUtils.getGenericTypeNames(params.map { case (_, _, typ) => typ })
      val typeParams = if (genericTypeNames.nonEmpty) s"<${genericTypeNames.mkString(", ")}>" else ""
      val functionParams = params.map { case (id, _, tpe) => s"var_$id: ${RustUtils.convertType(tpe)}" }
      val returnType = RustUtils.convertType(result.tpe)
      (s"fn fun_$id$typeParams(${functionParams.mkString(", ")}) -> $returnType {"
        +: rustNonStreamCodeGenerator
          .translateBody(body, result, rustNonStreamCodeGenerator.TypeArgManagement.empty, extSpec.spec.definitions)
        :+ "}")
    }
  }
}
