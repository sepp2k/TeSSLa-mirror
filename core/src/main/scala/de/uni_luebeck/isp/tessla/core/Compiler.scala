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

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.Compiler.Options
import de.uni_luebeck.isp.tessla.core.Tessla.TimeLiteral
import de.uni_luebeck.isp.tessla.core.TesslaAST.{Core, Typed}
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Result
import org.antlr.v4.runtime.CharStream

object Compiler {

  /**
   * Options for the [[Compiler]]
   *
    * @param baseTimeString the base time used to resolve time literals during translation
   * @param includeResolver function for resolving paths of `include` statements
   * @param stdlibIncludeResolver function for resolving `include`statements within the standard library
   * @param stdlibPath location of the standard library
   * @param flattenCore flag if the [[TesslaAST.Core]] code should be further flattened. This is only useful if the
   *                    code is required to be compliant to the language specification. Per default, this is skipped
   *                    for better readability.
   *
    * @see [[TimeUnit]]
   */
  case class Options(
    baseTimeString: Option[String] = None,
    includeResolver: String => Option[CharStream] = IncludeResolvers.fromFile,
    stdlibIncludeResolver: String => Option[CharStream] = IncludeResolvers.fromStdlibResource,
    stdlibPath: String = "stdlib.tessla",
    flattenCore: Boolean = false
  ) {
    lazy val baseTime: Option[TimeLiteral] =
      baseTimeString.map(TimeLiteral.fromString(_, Location.option("basetime")))
  }

  /**
   * Instantiates a compiler and run it.
   *
    * @param src the source to compile
   * @param options the compiler options to use
   * @return
   */
  def compile(src: CharStream, options: Options): Result[Core.Specification] = new Compiler(options).compile(src)
}

class Compiler(options: Options) {

  /**
   * Creates the full translation phase from the source to a
   * [[TesslaAST.Core.Specification]].
   *
    * @return the resulting translation phase
   *
    * @see [[tesslaToTyped]], [[typedToCore]] and [[coreToFlatCore]] for specific parts of the pipeline.
   */
  def instantiatePipeline: TranslationPhase[CharStream, Core.Specification] = {
    tesslaToTyped
      .andThen(typedToCore)
      .andThen(TranslationPhase.EnableIf(options.flattenCore, coreToFlatCore))
  }

  /** Creates a translation phase from the source to a [[TesslaAST.Typed.Specification]].
   *
    * This phase combines the following phases:
   *   - [[TesslaParser]]
   *   - [[TesslaSyntaxToTessla]]
   *   - [[Flattener]]
   *   - [[TypeChecker]]
   *   - [[TypedTessla2TesslaASTCore]]
   *
    * @return the resulting translation phase
   */
  def tesslaToTyped: TranslationPhase[CharStream, TesslaAST.Typed.Specification] = {
    new TesslaParser.WithIncludes(options.includeResolver)
      .andThen(new StdlibIncluder(options.stdlibIncludeResolver, options.stdlibPath))
      .andThen(TesslaSyntaxToTessla)
      .andThen(Flattener)
      .andThen(TypeChecker)
      .andThen(new TypedTessla2TesslaASTCore(options.baseTime))
  }

  /**
   * Creates a translation phase from a [[TesslaAST.Typed.Specification]] to a [[TesslaAST.Core.Specification]].
   *
    * This phase is consists only of the [[ConstantEvaluator]].
   *
    * @return the resulting translation phase
   */
  def typedToCore: TranslationPhase[Typed.Specification, Core.Specification] =
    (new ConstantEvaluator)
      .andThen(AnnotationValidator)

  /** Creates an identity phase on [[TesslaAST.Core.Specification]], which takes the given core representation and
   * flattens it further.
   *
    * This phase is skipped unless [[Compiler.Options.flattenCore]] is set, and is only required if the resulting
   * [[TesslaAST.Core.Specification]] has to be compliant to the language specification.
   *
    * @return the resulting translation phase
   */
  def coreToFlatCore: TranslationPhase[Core.Specification, Core.Specification] = FlattenCore

  /** Shorthand function which creates the translation phase and directly translates the given source with it.
   *
    * @param src the source to translate
   * @return the translation result
   *
    * @see [[instantiatePipeline]]
   */
  def compile(src: CharStream): Result[Core.Specification] = instantiatePipeline.translate(src)

}
