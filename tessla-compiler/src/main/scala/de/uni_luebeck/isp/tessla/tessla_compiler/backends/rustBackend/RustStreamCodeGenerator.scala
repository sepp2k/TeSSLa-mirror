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
import de.uni_luebeck.isp.tessla.tessla_compiler.{Diagnostics, StreamCodeGeneratorInterface}

class RustStreamCodeGenerator(rustNonStreamCodeGenerator: RustNonStreamCodeGenerator)
    extends StreamCodeGeneratorInterface[SourceSegments, Unit] {

  /**
   * Returns name if ea is an ExpressionRef otherwise an exception is thrown
   * @param ea The expression to be examined
   * @return Name of ea if it is an ExpressionRef
   */
  private def streamNameFromExpressionArg(ea: ExpressionArg): String = {
    ea match {
      case ExpressionRef(id, _, _) => "state.var_" + id.fullName
      case e: Expression =>
        throw Diagnostics
          .CoreASTError("Required ExpressionRef, but Expression found. Non flat AST.", e.location)
    }
  }

  private def createStreamContainer(
    stream_id: Identifier,
    stream_type: Type,
    init_expr: String,
    currSrc: SourceSegments
  ): String = {
    val name = s"var_${stream_id.fullName}"
    currSrc.stateDef.append(s"$name: EventContainer<${RustUtils.convertType(stream_type)}>")
    currSrc.stateInit.append(s"$name: $init_expr")
    val stream = s"state.$name"
    currSrc.store.append(s"$stream.update_last();")
    stream
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
  ): Unit = {
    createStreamContainer(output_id, output_type, "init()", currSrc)
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
  ): Unit = {
    val default = rustNonStreamCodeGenerator.translateExpressionArg(
      default_expr,
      rustNonStreamCodeGenerator.TypeArgManagement.empty
    )
    val output = createStreamContainer(output_id, output_type, s"init_with_value($default)", currSrc)
    val stream = streamNameFromExpressionArg(stream_expr)
    currSrc.computation.append(s"default(&mut $output, &$stream, state.current_ts);")
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
  ): Unit = {
    val output = createStreamContainer(output_id, output_type, "init()", currSrc)
    val stream = streamNameFromExpressionArg(stream_expr)
    val default = streamNameFromExpressionArg(default_expr)
    currSrc.computation.append(s"default_from(&mut $output, &$stream, &$default);")
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
  ): Unit = {
    val output = createStreamContainer(output_id, IntType, "init()", currSrc)
    val stream = streamNameFromExpressionArg(stream_expr)
    currSrc.computation.append(s"time(&mut $output, &$stream, state.current_ts);")
  }

  /**
   * Produces code for a x = last(...) expression
   *
   * @param output_id    The id last is assigned to
   * @param output_type  The type of id. Must be Events[...]
   * @param values_expr  The value-stream parameter of the last
   * @param trigger_expr The trigger/clock-stream parameter of the last
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceLastStepCode(
    output_id: Identifier,
    output_type: Type,
    values_expr: ExpressionArg,
    trigger_expr: ExpressionArg,
    currSrc: SourceSegments
  ): Unit = {
    val output = createStreamContainer(output_id, output_type, "init()", currSrc)
    val values = streamNameFromExpressionArg(values_expr)
    val trigger = streamNameFromExpressionArg(trigger_expr)
    currSrc.computation.append(s"last(&mut $output, &$values, &$trigger);")
  }

  /**
   * Produces code for a x = delay(...) expression
   *
   * @param output_id    The id delay is assigned to
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
  ): Unit = {
    val output = createStreamContainer(output_id, UnitType, "init()", currSrc)
    val delay = streamNameFromExpressionArg(delay_expr)
    val reset = streamNameFromExpressionArg(reset_expr)

    currSrc.stateDef.append(s"nextDelay_$output: i64")
    currSrc.stateInit.append(s"nextDelay_$output: 0")

    currSrc.timestamp.append(
      s"if state.nextDelay_$output > state.current_ts && state.nextDelay_$output < new_input_ts { state.current_ts = state.nextDelay_$output; }"
    )

    currSrc.computation.append(s"delay(&mut $output, &$delay, &$reset, state.nextDelay_$output, state.current_ts);")
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
  ): Unit = {
    val output = createStreamContainer(output_id, output_type, "init()", currSrc)
    val arguments = argument_exprs.map(streamNameFromExpressionArg).map(a => s"&$a")
    val function = rustNonStreamCodeGenerator.translateExpressionArg(
      function_expr,
      rustNonStreamCodeGenerator.TypeArgManagement.empty
    )
    currSrc.computation.append(s"lift${arguments.size}(&mut $output, ${arguments.mkString(", ")}, $function);")
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
  ): Unit = {
    val output = createStreamContainer(output_id, output_type, "init()", currSrc)
    val arguments = argument_exprs.map(streamNameFromExpressionArg).map(a => s"&$a")
    val function = rustNonStreamCodeGenerator.translateExpressionArg(
      function_expr,
      rustNonStreamCodeGenerator.TypeArgManagement.empty
    )
    currSrc.computation.append(s"slift${arguments.size}(&mut $output, ${arguments.mkString(", ")}, $function);")
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
  ): Unit = {
    val output = createStreamContainer(output_id, output_type, "init()", currSrc)
    val arguments = argument_exprs.map(streamNameFromExpressionArg).map(a => s"&$a")
    currSrc.computation.append(s"merge(&mut $output, vec![${arguments.mkString(", ")}]);")
  }

  /**
   * Produces code for a x = count(...) expression
   *
   * @param output_id        The id count is assigned to
   * @param count_stream_expr Expression of the stream to be counted
   * @param currSrc   The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceCountStepCode(
    output_id: Identifier,
    count_stream_expr: ExpressionArg,
    currSrc: SourceSegments
  ): Unit = {
    val output = createStreamContainer(output_id, IntType, "init_with_value(Value(0_i64))", currSrc)
    val count_stream = streamNameFromExpressionArg(count_stream_expr)
    currSrc.computation.append(s"count(&mut $output, &$count_stream);")
  }

  /**
   * Produces code for a x = const(...) expression
   *
   * @param output_id    The id const is assigned to
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
  ): Unit = {
    val output = createStreamContainer(output_id, output_type, "init()", currSrc)
    val value =
      rustNonStreamCodeGenerator.translateExpressionArg(value_expr, rustNonStreamCodeGenerator.TypeArgManagement.empty)
    val trigger = streamNameFromExpressionArg(trigger_expr)
    currSrc.computation.append(s"constant(&mut $output, $value, &$trigger);")
  }

  /**
   * Produces code for a x = filter(...) expression
   *
   * @param output_id      The id filter is assigned to
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
  ): Unit = {
    val output = createStreamContainer(output_id, output_type, "init()", currSrc)
    val value = streamNameFromExpressionArg(value_expr)
    val condition = streamNameFromExpressionArg(condition_expr)
    currSrc.computation.append(s"filter(&mut $output, &$value, &$condition);")
  }

  /**
   * Produces code for a x = fold(...) expression
   *
   * @param output_id     The id fold is assigned to
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
  ): Unit = {
    val init =
      rustNonStreamCodeGenerator.translateExpressionArg(init_expr, rustNonStreamCodeGenerator.TypeArgManagement.empty)
    val output = createStreamContainer(output_id, output_type, s"init_with_value($init)", currSrc)
    val stream = streamNameFromExpressionArg(stream_expr)
    val function = rustNonStreamCodeGenerator.translateExpressionArg(
      function_expr,
      rustNonStreamCodeGenerator.TypeArgManagement.empty
    )
    currSrc.computation.append(s"fold(&mut $output, &$stream, $function);")
  }

  /**
   * Produces code for a x = reduce(...) expression
   *
   * @param output_id     The id reduce is assigned to
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
  ): Unit = {
    val output = createStreamContainer(output_id, output_type, "init()", currSrc)
    val stream = streamNameFromExpressionArg(stream_expr)
    val function = rustNonStreamCodeGenerator.translateExpressionArg(
      function_expr,
      rustNonStreamCodeGenerator.TypeArgManagement.empty
    )
    currSrc.computation.append(s"reduce(&mut $output, &$stream, $function);")
  }

  /**
   * Produces code for a x = unitIf(...) expression
   *
   * @param output_id      The id unitIf is assigned to
   * @param condition_expr Expression of the stream with the condition for unitIf
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  override def produceUnitIfStepCode(
    output_id: Identifier,
    condition_expr: ExpressionArg,
    currSrc: SourceSegments
  ): Unit = {
    val output = createStreamContainer(output_id, UnitType, "init()", currSrc)
    val condition = streamNameFromExpressionArg(condition_expr)
    currSrc.computation.append(s"unitIf(&mut $output, &$condition);")
  }

  /**
   * Produces code for a x = pure(...) expression
   *
   * @param output_id   The id pure is assigned to
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
  ): Unit = {
    val output = createStreamContainer(output_id, output_type, "init()", currSrc)
    val stream = streamNameFromExpressionArg(stream_expr)
    currSrc.computation.append(s"pure(&mut $output, &$stream);")
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
  ): Unit = {
    throw Diagnostics.CommandNotSupportedError(s"The translation of native:$name(...) is not implemented.")
  }

  /**
   * Add code for output generation to the source segments.
   * The output gets value, error, timestamp passed and if the printing format is raw (i.e. only value, not the
   * current timestamp) and has to be translated accordingly in the final code generation
   *
   * @param id The id of the stream to be printed
   * @param typ id's type. Must be Events[...]
   * @param nameOpt The alias name of id for printing. Optional.
   * @param raw If the output should be printed raw (without timestamp). Is passed to __[TC]output__.
   * @param srcSegments The source segments the generated block is added to.
   *                There is code attached to the output segment.
   */
  def produceOutputCode(
    id: Identifier,
    typ: Type,
    nameOpt: Option[String],
    srcSegments: SourceSegments,
    raw: Boolean,
    ioInterface: Boolean
  ): Unit = {
    val s = s"var_${id.fullName}"
    val t = RustUtils.convertType(typ)
    // FIXME: All this replacing of specific chars is probably not exhaustive and kinda not nice to do here
    var name = nameOpt.getOrElse(id.idOrName.left.getOrElse(id.fullName))
    val cleanName = RustUtils.NON_ALPHA_PATTERN.replaceAllIn(name, m => s"χ${m.group(0).charAt(0).asInstanceOf[Int]}")
    name = name.replace("$", "\\$")

    srcSegments.stateDef.append(s"out_$cleanName: Option<fn($t, i64)> /* $name */")

    if (ioInterface) {
      val nameString = name
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\"", "\\\"")

      // FIXME better to string conversion??
      srcSegments.stateInit.append(
        s"""out_$cleanName: Some(|value, ts| output_var(value, \"$nameString\", ts, $raw))"""
      )
    } else {
      srcSegments.stateInit.append(s"out_$cleanName: None")
    }
    srcSegments.computation.append(s"state.$s.call_output(state.out_$cleanName, state.current_ts);")
  }

  /**
   * Produces code reading the input (variable inputStream and value) and storing it.
   *
   * @param stream_id   The input stream to be handled
   * @param stream_type The input stream's type. Must be Events[...].
   * @param srcSegments The source segments the generated block is added to.
   *                    There is code attached to the input, and output section.
   */
  def produceInputCode(
    stream_id: Identifier,
    stream_type: Type,
    srcSegments: SourceSegments,
    ioInterface: Boolean
  ): Unit = {
    val stream = createStreamContainer(stream_id, stream_type, "init()", srcSegments)

    val t = RustUtils.convertType(stream_type)

    srcSegments.stateDef.append(s"set_$stream_id: fn($t, i64, &mut State)")

    if (ioInterface) {
      srcSegments.input.append(
        s"""if input_stream_name == \"$stream_id\" { (state.set_$stream_id)($t::from(input_stream_value.as_str()), new_input_ts, state); }"""
      )
    }

    srcSegments.stateInit.append(s"""set_$stream_id: |value: $t, ts: i64, state: &mut State| {
                                    |$stream.set_event(value);
                                    |}""".stripMargin)
  }
}
