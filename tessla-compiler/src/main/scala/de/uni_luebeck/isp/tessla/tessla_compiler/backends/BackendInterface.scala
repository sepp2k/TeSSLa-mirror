/*
 * Copyright 2020 Institute of Software Engineering and Programming Languages,
 *                University of Lübeck, Germany
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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.{
  IntermediateCode,
  IntermediateCodeTypeInference,
  IntermediateCodeUtils
}

import scala.io.Source

/**
 * Abstract base class for the translation from intermediate code to real source code
 * @param sourceTemplate Resource path of the template code, where generated code is inserted
 */
abstract class BackendInterface(sourceTemplate: String) extends TranslationPhase[SourceListing, String] {

  protected var variables: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)] = Map()

  /**
   * Function triggering the translation from a [[IntermediateCode.SourceListing]] to real-world source code as String.
   * Therefore the sections of the [[IntermediateCode.SourceListing]] are translated to code and included at special places of
   * the template code by replacing
   *
   * - //VARDEF by a section where all used variables are defined
   * - //TRIGGER by the translation of tsGenSource
   * - //STEP by the translation of stepSource
   * - //TAIL by the translation of tailSource
   * - //INPUTPROCESSING by the translation of inputProcessing
   * - //STATIC by the translation of staticSource
   *
   * For more details on the requirements of the template see the markdown documents in doc
   *
   * @param orgListing The source listing to be translated
   * @return The source code of the generated monitor program
   */
  def translate(orgListing: SourceListing): Result[String] = {
    val nonStaticVars = IntermediateCodeUtils.getVariableMap(
      orgListing.tsGenSource
        .concat(orgListing.stepSource)
        .concat(orgListing.inputProcessing)
        .concat(orgListing.tailSource)
    ) ++
      Map(
        "inputStream" -> (StringType, Some(StringValue("")), false),
        "value" -> (StringType, Some(StringValue("")), false),
        "currTs" -> (LongType, Some(LongValue(0)), false),
        "lastProcessedTs" -> (LongType, Some(LongValue(0)), false),
        "newInputTs" -> (LongType, Some(LongValue(0)), false)
      )

    val staticVars = IntermediateCodeUtils.getVariableMap(orgListing.staticSource)

    variables = nonStaticVars ++ staticVars

    val listing =
      IntermediateCodeTypeInference.generateCodeWithCasts(orgListing, variables.map { case (n, (t, _, _)) => (n, t) })

    val source = Source.fromResource(sourceTemplate).mkString
    val rewrittenSource = source
      .replace("//VARDEF", generateVariableDeclarations(nonStaticVars).mkString("\n"))
      .replace("//TRIGGER", generateCode(listing.tsGenSource))
      .replace("//STEP", generateCode(listing.stepSource))
      .replace("//TAIL", generateCode(listing.tailSource))
      .replace("//INPUTPROCESSING", generateCode(listing.inputProcessing))
      .replace(
        "//STATIC",
        generateVariableDeclarations(staticVars).mkString("\n") + "\n\n" + generateCode(listing.staticSource)
      )

    Success(rewrittenSource, Seq())
  }

  /**
   * Translates a map of variables with base information (type, default value, lazy assignment) to corresponding
   * variable declarations in the target source code.
   * Has to be implemented by Backend-Implementations.
   * @param vars Map of variables to be translated: Name -> (Type x Default value x Lazy assignment)
   * @return Sequence of variable definitions
   */
  def generateVariableDeclarations(vars: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)]): Seq[String]

  /**
   * Translates a sequence of [[IntermediateCode.ImpLanStmt]] to the corresponding code in the target language.
   * Has to be implemented by Backend-Implementations.
   * @param stmts The sequence of statements to be translated.
   * @return The generated code in the target language
   */
  def generateCode(stmts: Seq[ImpLanStmt]): String
}
