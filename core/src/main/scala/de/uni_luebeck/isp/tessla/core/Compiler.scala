/*
 * Copyright (c) 2020 Institute of Software Engineering and Programming Languages,
 * University of Lübeck, Germany
 *
 * Modified MIT license:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this binary (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software and the code which is
 * generated by the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
  def typedToCore: TranslationPhase[Typed.Specification, Core.Specification] = new ConstantEvaluator

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
