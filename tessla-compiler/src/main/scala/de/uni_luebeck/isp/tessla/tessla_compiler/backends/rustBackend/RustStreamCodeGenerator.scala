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
import de.uni_luebeck.isp.tessla.tessla_compiler.{Diagnostics, NonStreamCodeGenerator, StreamCodeGeneratorInterface}

class RustStreamCodeGenerator(rustCodeGenerator: RustCodeGenerator)
    extends StreamCodeGeneratorInterface[SourceSegments] {

  /**
   * Returns name if ea is an ExpressionRef otherwise an exception is thrown
   * @param ea The expression to be examined
   * @return Name of ea if it is an ExpressionRef
   */
  private def streamNameFromExpressionArg(ea: ExpressionArg): String = {
    ea match {
      case ExpressionRef(id, _, _) => "var_" + id.fullName
      case e: Expression =>
        throw Diagnostics
          .CoreASTError("Required ExpressionRef, but Expression found. Non flat AST.", e.location)
    }
  }

  /**
   * Produces code for a x = nil expression
   *
   * @param output_id    The id nil is assigned to
   * @param output_type  The type of id. Must be Events[...]
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceNilStepCode(
    output_id: Identifier,
    output_type: Type,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let $output = init();")
    currSrc
  }

  /**
   * Produces code for a x = default(...) expression
   *
   * @param output_id    The id default is assigned to
   * @param output_type  The type of id. Must be Events[...]
   * @param stream_expr  The stream parameter of the default
   * @param default_expr The default parameter of the default
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceDefaultStepCode(
    output_id: Identifier,
    output_type: Type,
    stream_expr: ExpressionArg,
    default_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    val default = rustCodeGenerator.translateExpressionArg(default_expr, rustCodeGenerator.TypeArgManagement.empty)
    currSrc.variables.append(s"let mut $output = init_with_value($default);")
    val stream = streamNameFromExpressionArg(stream_expr)
    currSrc.computation.append(s"default(&$output, &$stream);")
    currSrc
  }

  /**
   * Produces code for a x = defaultFrom(...) expression
   *
   * @param output_id    The id defaultFrom is assigned to
   * @param output_type  The type of id. Must be Events[...]
   * @param stream_expr  The stream parameter of the defaultFrom
   * @param default_expr The stream parameter of the defaultFrom
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceDefaultFromStepCode(
    output_id: Identifier,
    output_type: Type,
    stream_expr: ExpressionArg,
    default_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val stream = streamNameFromExpressionArg(stream_expr)
    val default = streamNameFromExpressionArg(default_expr)
    currSrc.computation.append(s"defaultFrom(&$output, &$stream, &$default);")
    currSrc
  }

  /**
   * Produces code for a x = time(...) expression
   *
   * @param output_id    The id time is assigned to
   * @param stream_expr  The base-stream parameter of the time
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceTimeStepCode(
    output_id: Identifier,
    stream_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val stream = streamNameFromExpressionArg(stream_expr)
    currSrc.computation.append(s"time(&$output, &$stream);")
    currSrc
  }

  /**
   * Produces code for a x = last(...) expression
   *
   * @param output_id    The id last is assigned to
   * @param output_type  The type of id. Must be Events[...]
   * @param values_expr  The value-stream parameter of the last
   * @param trigger_expr   The trigger/clock-stream parameter of the last
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceLastStepCode(
    output_id: Identifier,
    output_type: Type,
    values_expr: ExpressionArg,
    trigger_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val values = streamNameFromExpressionArg(values_expr)
    val trigger = streamNameFromExpressionArg(trigger_expr)
    currSrc.computation.append(s"last(&$output, &$values, &$trigger);")
    // TODO this will needs deduplication/different representation:
    currSrc.store.append(s"update_last(&$output);")
    currSrc
  }

  /**
   * Produces code for a x = delay(...) expression
   *
   * @param output_id      The id delay is assigned to
   * @param delay_expr   The delay-stream parameter of the delay
   * @param reset_expr   The reset-stream parameter of the delay
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceDelayStepCode(
    output_id: Identifier,
    delay_expr: ExpressionArg,
    reset_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val delay = streamNameFromExpressionArg(delay_expr)
    val reset = streamNameFromExpressionArg(reset_expr)
    currSrc.computation.append(s"delay(&$output, &$delay, &$reset);")

    /** TODO needs additional rust data-structure/timestamp code
     * This needs to do something like this:
     * if ${output}_nextTs > lastProcessedTs && currTs > ${output}_nextTs {
     *   currTs = ${output}_nextTs;
     * }
     */
    currSrc.timestamp.append(s"interrupt_for_delay(&$output);")

    /** TODO is there a reason for this being put /after/ the processing part?
     * if ${stream}_changed {
     *   if ${output}_changed && ${reset}_changed {
     *     ${output}_nextTs = currTs + ${stream}_value
     *   } else if (${reset}_changed) {
     *     ${output}_nextTs = -1 //TODO this is supposed to stop the delay?
     *   }
     * }
     */
    currSrc.output.append(s"reset_delay(&$output, &$delay, &$reset);")
    currSrc
  }

  /**
   * Produces code for a x = lift(...) expression
   *
   * @param output_id      The id lift is assigned to
   * @param output_type    The type of id. Must be Events[...]
   * @param argument_exprs The arguments of the lift except the last one (function)
   * @param function_expr  The lifted function
   * @param currSrc  The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceLiftStepCode(
    output_id: Identifier,
    output_type: Type,
    argument_exprs: Seq[ExpressionArg],
    function_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val arguments = argument_exprs.map(streamNameFromExpressionArg).map(a => s"&$a").mkString(", ")
    val function = rustCodeGenerator.translateExpressionArg(function_expr, rustCodeGenerator.TypeArgManagement.empty);
    // TODO I think the lifted function expects the stream values to be wrapped in options?
    // TODO function can be different things, we'll probably need some preprocessing (nonStream.translateFunctionCall)
    currSrc.computation.append(s"lift(&$output, vec![$arguments], $function);")
    currSrc
  }

  /**
   * Produces code for a x = slift(...) expression
   *
   * @param output_id       The id slift is assigned to
   * @param output_type       The type of id. Must be Events[...]
   * @param argument_exprs     The arguments of the lift except the last one (function)
   * @param function_expr The lifted function
   * @param currSrc  The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceSignalLiftStepCode(
    output_id: Identifier,
    output_type: Type,
    argument_exprs: Seq[ExpressionArg],
    function_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val arguments = argument_exprs.map(streamNameFromExpressionArg).map(a => s"&$a").mkString(", ")
    val function = rustCodeGenerator.translateExpressionArg(function_expr, rustCodeGenerator.TypeArgManagement.empty)
    currSrc.computation.append(s"slift(&$output, vec![$arguments], $function);")
    currSrc
  }

  /**
   * Produces code for a x = merge(...) expression
   *
   * @param output_id      The id merge is assigned to
   * @param output_type      The type of id. Must be Events[...]
   * @param argument_exprs    The stream expressions to be merged
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceMergeStepCode(
    output_id: Identifier,
    output_type: Type,
    argument_exprs: Seq[ExpressionArg],
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val arguments = argument_exprs.map(streamNameFromExpressionArg).map(a => s"&$a").mkString(", ")
    currSrc.computation.append(s"merge(&$output, vec![$arguments]);")
    currSrc
  }

  /**
   * Produces code for a x = count(...) expression
   *
   * @param output_id        The id merge is assigned to
   * @param count_stream_expr Expression of the stream to be counted
   * @param currSrc   The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceCountStepCode(
    output_id: Identifier,
    count_stream_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init_with_value(0_i64);")
    val count_stream = streamNameFromExpressionArg(count_stream_expr)
    currSrc.computation.append(s"count(&$output, $count_stream);")
    currSrc
  }

  /**
   * Produces code for a x = const(...) expression
   *
   * @param output_id    The id merge is assigned to
   * @param output_type  Type of the output stream
   * @param value_expr   Argument expressions for const value
   * @param trigger_expr Argument expression for triggering stream
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceConstStepCode(
    output_id: Identifier,
    output_type: Type,
    value_expr: ExpressionArg,
    trigger_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val value = rustCodeGenerator.translateExpressionArg(value_expr, rustCodeGenerator.TypeArgManagement.empty)
    val trigger = streamNameFromExpressionArg(trigger_expr)
    currSrc.computation.append(s"const(&$output, $value, &$trigger);")
    currSrc
  }

  /**
   * Produces code for a x = filter(...) expression
   *
   * @param output_id      The id merge is assigned to
   * @param output_type    Type of the output stream
   * @param value_expr     Argument expressions for filter value stream
   * @param condition_expr Argument expressions for filter condition stream
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceFilterStepCode(
    output_id: Identifier,
    output_type: Type,
    value_expr: ExpressionArg,
    condition_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val value = streamNameFromExpressionArg(value_expr)
    val condition = streamNameFromExpressionArg(condition_expr)
    currSrc.computation.append(s"filter(&$output, &$value, &$condition);")
    currSrc
  }

  /**
   * Produces code for a x = fold(...) expression
   *
   * @param output_id     The id merge is assigned to
   * @param output_type   Type of the output stream
   * @param stream_expr   Expression of the stream to be folded
   * @param init_expr     Expression of the initial value for the folding
   * @param function_expr Expression of the function used for folding
   * @param currSrc  The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceFoldStepCode(
    output_id: Identifier,
    output_type: Type,
    stream_expr: ExpressionArg,
    init_expr: ExpressionArg,
    function_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    val init = rustCodeGenerator.translateExpressionArg(init_expr, rustCodeGenerator.TypeArgManagement.empty)
    currSrc.variables.append(s"let mut $output = init_with_value($init);")
    val stream = streamNameFromExpressionArg(stream_expr)
    val function = rustCodeGenerator.translateExpressionArg(function_expr, rustCodeGenerator.TypeArgManagement.empty)
    currSrc.computation.append(s"fold(&$output, &$stream, $function);")
    currSrc
  }

  /**
   * Produces code for a x = reduce(...) expression
   *
   * @param output_id     The id merge is assigned to
   * @param output_type   Type of the output stream
   * @param stream_expr   Expression of the stream to be reduced
   * @param function_expr Expression of the function used for reducing
   * @param currSrc  The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceReduceStepCode(
    output_id: Identifier,
    output_type: Type,
    stream_expr: ExpressionArg,
    function_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val stream = streamNameFromExpressionArg(stream_expr)
    val function = rustCodeGenerator.translateExpressionArg(function_expr, rustCodeGenerator.TypeArgManagement.empty)
    currSrc.computation.append(s"reduce(&$output, &$stream, $function);")
    currSrc
  }

  /**
   * Produces code for a x = unitIf(...) expression
   *
   * @param output_id      The id merge is assigned to
   * @param condition_expr Expression of the stream with the condition for unitIf
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceUnitIfStepCode(
    output_id: Identifier,
    condition_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val condition = streamNameFromExpressionArg(condition_expr)
    currSrc.computation.append(s"unitIf(&$output, &$condition);")
    currSrc
  }

  /**
   * Produces code for a x = pure(...) expression
   *
   * @param output_id   The id merge is assigned to
   * @param output_type Type of the output stream
   * @param stream_expr Stream to be filtered
   * @param currSrc   The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def producePureStepCode(
    output_id: Identifier,
    output_type: Type,
    stream_expr: ExpressionArg,
    currSrc: SourceSegments
  ): SourceSegments = {
    val output = s"var_${output_id.fullName}"
    currSrc.variables.append(s"let mut $output = init();")
    val stream = streamNameFromExpressionArg(stream_expr);
    currSrc.computation.append(s"pure(&$output, &$stream);")
    currSrc
  }

  /**
   * Produces code for a x = native:&lt;name&gt;(...) expression
   *
   * @param output_id      The id which is assigned (must be of stream type)
   * @param output_type    The Type of the output stream
   * @param name           The function name which is applied with args and typeArgs and then assigned
   * @param argument_exprs The arguments passed to e
   * @param argument_types The type arguments passed to e
   * @param currSource The source listing where the generated code is attached to.
   * @return The modified source listing
   */
  override def produceNativeFunctionStepCode(
    output_id: Identifier,
    output_type: Type,
    name: String,
    argument_exprs: Seq[ExpressionArg],
    argument_types: Seq[Type],
    currSource: SourceSegments
  ): SourceSegments = {
    throw Diagnostics.CommandNotSupportedError(s"The translation of native:$name(...) is not implemented.")
  }

  /**
   * Add code for output generation to the source segments.
   * The output gets value, error, timestamp passed and if the printing format is raw (i.e. only value, not the
   * current timestamp) and has to be translated accordingly in the final code generation
   *
   * @param id The id of the stream to be printed
   * @param t id's type. Must be Events[...]
   * @param nameOpt The alias name of id for printing. Optional.
   * @param raw If the output should be printed raw (without timestamp). Is passed to __[TC]output__.
   * @param srcSegments The source segments the generated block is added to.
   *                There is code attached to the output segment.
   */
  def produceOutputCode(
    id: Identifier,
    t: Type,
    nameOpt: Option[String],
    srcSegments: SourceSegments,
    raw: Boolean
  ): Unit = {
    val s = s"var_${id.fullName}"
    val name = nameOpt.getOrElse(id.idOrName.left.getOrElse(id.fullName)).replace("\n", "\\n").replace("\r", "\\r")

    // FIXME better to string conversion??

    srcSegments.output.append(s"""output_stream($s, "$name", currTs, $raw);""")
  }

  /**
   * Produces code reading the input (variable inputStream and value) and storing it.
   *
   * @param inStream The input stream to be handled
   * @param typ      The input stream's type. Must be Events[...].
   * @param srcSegments  The source segments the generated block is added to.
   *                 There is code attached to the input, and output section.
   */
  def produceInputCode(inStream: Identifier, typ: Type, srcSegments: SourceSegments): Unit = {
    val s = s"var_${inStream.fullName}"

    srcSegments.input.append(s"""if inputStream == "${inStream.idOrName.left.get}" { get_value_from_input($s) }""")

    // TODO not necessary for all streams:
    // srcSegments.output.append(s"update_last($s);")
  }
}
