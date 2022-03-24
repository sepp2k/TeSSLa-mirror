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

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Result
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.tessla_compiler.{DefinitionOrdering, Diagnostics, ExtendedSpecification}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

/**
 * Class implementing TranslationPhase for the translation from TeSSLa Core to
 * rust code
 * The translation of stream functions is performed in RustStreamCodeGenerator
 * The translation of other expressions in RustNonStreamCodeGenerator
 * @param ioInterface Indicates whether the generated code shall be able to read/write from/to stdio
 */
class TesslaCoreToRust(ioInterface: Boolean) extends TranslationPhase[ExtendedSpecification, String] {
  private val monitorTemplate: String = "de/uni_luebeck/isp/tessla/rust/templates/monitor_skeleton.rs"
  private val mainTemplate: String = "de/uni_luebeck/isp/tessla/rust/templates/main_skeleton.rs"

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

    private def produceOutputCode(
      outputMap: Map[Identifier, List[Annotations]],
      outputNames: mutable.Set[String],
      id: Identifier,
      srcSegments: SourceSegments
    ): Unit = {
      if (outputMap.contains(id)) {
        outputMap(id).foreach { annotations =>
          val name = TesslaAST.Core.getOutputName(annotations).getOrElse(id.idOrName.left.getOrElse(id.fullName))
          val raw = annotations.contains("raw")
          rustStreamCodeGenerator
            .produceOutputCode(id, getStreamType(id), name, srcSegments, raw, ioInterface, outputNames)
        }
      }
    }

    override protected def translateSpec(): String = {
      val srcSegments = SourceSegments()

      val outputMap = extSpec.spec.out.groupMap { case (expr, _) => expr.id } { case (_, annotations) => annotations }
      val outputNames = mutable.Set[String]()

      // Produce computation section
      DefinitionOrdering.order(extSpec.spec.definitions).foreach {
        case (Identifier((None, Some(name), _)), ExternExpression("[rust]Struct", RecordType(entries, _), _)) =>
          srcSegments.static.append(
            rustNonStreamCodeGenerator
              .translateStructDefinition(name, entries.toSeq.map { case (name, (typ, _)) => (name, typ) })
          )

        case (id, definition) =>
          definition.tpe match {
            case InstantiatedType("Events", _, _) =>
              rustStreamCodeGenerator
                .translateStreamDefinitionExpression(id, definition, extSpec.spec.definitions, srcSegments)
              produceOutputCode(outputMap, outputNames, id, srcSegments)
            case FunctionType(_, _, _, _) =>
              srcSegments.static.appendAll(rustNonStreamCodeGenerator.translateStaticFunction(id, definition))
            case _ =>
              srcSegments.lazyStatic.append(rustNonStreamCodeGenerator.translateStaticAssignment(id, definition))
          }
      }

      // Produce input consumption
      extSpec.spec.in.foreach {
        case (id, (typ, _)) =>
          rustStreamCodeGenerator.produceInputCode(id, typ, srcSegments, ioInterface)
          produceOutputCode(outputMap, outputNames, id, srcSegments)
      }

      insertSegments(srcSegments)
      // Rust does not like $ in names
        .replace("$", "京")
        .replace("\\京", "$")
    }

    private def insertSegments(srcSegments: SourceSegments): String = {
      val source = Source.fromResource(monitorTemplate).mkString
      val rewrittenSource = source
        .replace("//USERINCLUDES", "") //srcSegments.userIncludes.mkString(",\n)")) //TODO
        .replace("//STATEDEF", srcSegments.stateDef.mkString(",\n"))
        .replace("//LAZYSTATIC", srcSegments.lazyStatic.mkString("\n"))
        .replace("//STATIC", srcSegments.static.mkString("\n"))
        .replace("//STATEINIT", srcSegments.stateInit.mkString(",\n"))
        .replace("//STORE", srcSegments.store.mkString("\n"))
        .replace("//TIMESTAMP", srcSegments.timestamp.mkString("\n"))
        .replace("//COMPUTATION", srcSegments.computation.mkString("\n"))
        .replace("//INPUT", srcSegments.input.mkString("\n"))
        .replace("//DELAYRESET", srcSegments.delayreset.mkString("\n"))
      if (ioInterface) {
        rewrittenSource.replace("//ENDMAIN", "")
      } else {
        "fn main[\\s\\S]+//ENDMAIN".r.replaceFirstIn(rewrittenSource, "")
      }
    }
  }
}
