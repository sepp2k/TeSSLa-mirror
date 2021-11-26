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

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success, Translator}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.SourceListing

import scala.annotation.tailrec

/**
 * Class implementing TranslationPhase for the translation from TeSSLa Core to
 * abstract imperative code
 * The translation of stream functions is performed in StreamCodeGenerator
 * The translation of other expressions in NonStreamCodeGenerator
 * @param ioInterface Indicates whether the generated code shall be able to read/write from/to stdio
 */
class TesslaCoreToIntermediate(ioInterface: Boolean)
    extends TranslationPhase[ExtendedSpecification, (SourceListing, Set[String])] {

  /**
   * Function triggering the translation from a TeSSLa AST to a SourceListing of ImpLan statements
   *
   * @param extSpec The TeSSLa Core AST to be translated
   * @return A SourceListing containing imperative code to be translated into concrete syntax and included in a source
   *         skeleton
   */
  override def translate(extSpec: ExtendedSpecification): Result[(SourceListing, Set[String])] =
    new Translator(extSpec).translate()

  class Translator(extSpec: ExtendedSpecification) extends TranslationPhase.Translator[(SourceListing, Set[String])] {

    val nonStreamCodeGenerator = new NonStreamCodeGenerator(extSpec)
    val streamCodeGenerator = new StreamCodeGenerator(nonStreamCodeGenerator)

    private val spec = extSpec.spec
    private val in = spec.in
    private val definitions = spec.definitions
    private val out = spec.out

    protected def getStreamType(id: Identifier): Type = {
      if (definitions.contains(id)) {
        definitions(id).tpe
      } else if (in.contains(id)) {
        in(id)._1
      } else {
        throw Diagnostics.CoreASTError(s"Type of stream $id cannot be found")
      }
    }

    override protected def translateSpec(): (SourceListing, Set[String]) = {
      var currSrc = SourceListing(Seq(), Seq(), Seq(), Seq(), Seq())
      var noRemove = Set[String]()

      //Produce calculation section
      DefinitionOrdering.order(definitions).foreach {
        case (id, definition) =>
          currSrc = definition.tpe match {
            case InstantiatedType("Events", _, _) =>
              streamCodeGenerator.translateStreamDefinitionExpression(id, definition, definitions, currSrc)
            case _ =>
              SourceListing(
                currSrc.stepSource,
                currSrc.tailSource,
                currSrc.tsGenSource,
                currSrc.inputProcessing,
                currSrc.staticSource :+ nonStreamCodeGenerator
                  .translateAssignment(id, definition, nonStreamCodeGenerator.TypeArgManagement.empty, definitions)
              )
          }
      }

      //Produce output generation
      out.foreach { o =>
        val name = TesslaAST.Core.getOutputName(o._2)

        if (ioInterface) {
          currSrc = streamCodeGenerator.produceOutputToConsoleCode(
            o._1.id,
            getStreamType(o._1.id),
            name,
            currSrc,
            o._2.contains("raw")
          )
        } else {
          currSrc = streamCodeGenerator.produceOutputToAPICode(
            o._1.id,
            getStreamType(o._1.id),
            name,
            currSrc,
            o._2.contains("raw")
          )
        }
      }

      //Produce input consumption
      in.foreach { i =>
        currSrc = streamCodeGenerator.produceInputUnchangeCode(i._1, currSrc)
        if (ioInterface) {
          currSrc = streamCodeGenerator.produceInputFromConsoleCode(i._1, i._2._1, currSrc)
        } else {
          currSrc = streamCodeGenerator.produceInputFromAPICode(i._1, i._2._1, currSrc)
          noRemove += s"set_var_${i._1.fullName}"
        }
      }

      (currSrc, noRemove)
    }

  }

}
