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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils._
import de.uni_luebeck.isp.tessla.tessla_compiler.{
  IntermediateCode,
  IntermediateCodeTypeInference,
  IntermediateCodeUtils
}

import scala.io.Source

/**
 * Abstract base class for the translation from intermediate code to real source code
 *
 * @param sourceTemplate Resource path of the template code, where generated code is inserted
 * @param userIncludes Additional user-specific code which is inserted at the //USERINCLUDES comment in the source template.
 *                     Can be used e.g. for additional includes.
 */
abstract class BackendInterface(sourceTemplate: String, userIncludes: String)
    extends TranslationPhase[SourceListing, String] {

  protected var variables: Seq[(String, (ImpLanType, Option[ImpLanExpr], DeclarationType))] = Seq()

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
    val nonStaticVars = IntermediateCodeUtils.getVariableSeq(
      orgListing.tsGenSource
        .concat(orgListing.stepSource)
        .concat(orgListing.inputProcessing)
        .concat(orgListing.tailSource),
      Seq(
        "inputStream" -> (StringType, Some(StringValue("")), VariableDeclaration),
        "value" -> (StringType, Some(StringValue("")), VariableDeclaration),
        "currTs" -> (LongType, Some(LongValue(0)), VariableDeclaration),
        "lastProcessedTs" -> (LongType, Some(LongValue(0)), VariableDeclaration),
        "newInputTs" -> (LongType, Some(LongValue(0)), VariableDeclaration)
      ),
      true
    )
    var staticVars = IntermediateCodeUtils.getVariableSeq(orgListing.staticSource, global = true)

    variables = nonStaticVars ++ staticVars

    val listing =
      IntermediateCodeTypeInference.generateCodeWithCasts(
        orgListing,
        variables.map { case (n, (t, _, _)) => (n, t) }.toMap
      )

    staticVars = IntermediateCodeUtils.getVariableSeq(listing.staticSource, global = true)

    val source = Source.fromResource(sourceTemplate).mkString
    val rewrittenSource = source
      .replace(
        "//VARDEF",
        generateVariableDeclarations(nonStaticVars.diff(staticVars)).mkString("\n")
      )
      .replace("//TRIGGER", generateCode(listing.tsGenSource))
      .replace("//STEP", generateCode(listing.stepSource))
      .replace("//TAIL", generateCode(listing.tailSource))
      .replace("//INPUTPROCESSING", generateCode(listing.inputProcessing))
      .replace(
        "//STATIC",
        generateVariableDeclarations(staticVars).mkString("\n") + "\n\n" + generateCode(
          listing.staticSource
        )
      )
      .replace("//USERINCLUDES", userIncludes)

    Success(rewrittenSource, Seq())
  }

  /**
   * Translates a set of variables with base information (type, default value, declaration type) to corresponding
   * variable declarations in the target source code.
   * Has to be implemented by Backend-Implementations.
   * @param vars Sequence of variables to be translated: Name -> (Type x Default value x Declaration type)
   * @return Sequence of variable definitions
   */
  def generateVariableDeclarations(vars: Seq[(String, (ImpLanType, Option[ImpLanExpr], DeclarationType))]): Seq[String]

  /**
   * Translates a sequence of [[IntermediateCode.ImpLanStmt]] to the corresponding code in the target language.
   * Has to be implemented by Backend-Implementations.
   * @param stmts The sequence of statements to be translated.
   * @return The generated code in the target language
   */
  def generateCode(stmts: Seq[ImpLanStmt]): String
}
