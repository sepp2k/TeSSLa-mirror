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

import scala.annotation.tailrec

/**
 * Abstract base class for the translation of stream code
 */
abstract class StreamCodeGeneratorInterface[CollectionType] {

  /**
   * Translates an assignment to a stream variable and attaches it in a given
   * source collection. The assigned expression has to be (and is always in TeSSLa Core) an application.
   *
   * @param id The id which is assigned (must be of stream type)
   * @param e The expression which is applied with args and typeArgs and then assigned
   * @param args The arguments passed to e
   * @param typeArgs The type arguments passed to e
   * @param currSource The source listing where the generated code is attached to.
   * @return The modified source listing
   */
  def translateExternSignalExpression(
    id: Identifier,
    e: ExternExpression,
    args: Seq[ExpressionArg],
    typeArgs: Seq[Type],
    currSource: CollectionType
  ): CollectionType = {
    val typ = e.tpe.asInstanceOf[FunctionType]
    val typeParamMap = typ.typeParams.zip(typeArgs).toMap
    e.name match {
      case "nil" =>
        produceNilStepCode(id, typ.resultType.resolve(typeParamMap), currSource)
      case "default" =>
        produceDefaultStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
      case "defaultFrom" =>
        produceDefaultFromStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
      case "time" =>
        produceTimeStepCode(id, args(0), currSource)
      case "last" =>
        produceLastStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
      case "delay" =>
        produceDelayStepCode(id, args(0), args(1), currSource)
      case "lift" =>
        produceLiftStepCode(id, typ.resultType.resolve(typeParamMap), args.dropRight(1), args.last, currSource)
      case "slift" =>
        produceSignalLiftStepCode(id, typ.resultType.resolve(typeParamMap), args.dropRight(1), args.last, currSource)
      case "merge" =>
        produceMergeStepCode(id, typ.resultType.resolve(typeParamMap), args, currSource)
      case "count" =>
        produceCountStepCode(id, args(0), currSource)
      case "const" =>
        produceConstStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
      case "filter" =>
        produceFilterStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
      case "fold" =>
        produceFoldStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), args(2), currSource)
      case "reduce" =>
        produceReduceStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
      case "unitIf" =>
        produceUnitIfStepCode(id, args(0), currSource)
      case "pure" =>
        producePureStepCode(id, typ.resultType.resolve(typeParamMap), args(0), currSource)
      case name if name.startsWith("native:") =>
        produceNativeFunctionStepCode(id, typ.resultType.resolve(typeParamMap), e.name, args, typeArgs, currSource)
      case _ => throw Diagnostics.CommandNotSupportedError(e.toString)
    }
  }

  @tailrec
  final protected def externResolution(
    e: ExpressionArg,
    definitions: Map[Identifier, DefinitionExpression]
  ): ExternExpression = e match {
    case e: ExternExpression => e
    case ExpressionRef(id, _, _) if definitions.contains(id) =>
      externResolution(definitions(id), definitions)
    case _ =>
      throw Diagnostics.CoreASTError(
        "No extern or reference to extern in function application with stream result",
        e.location
      )
  }

  /**
   * Translates a definition expression, which is always an assignment of an external signal application
   * to a stream variable and attaches it in a given source collection.
   *
   * @param id The id which is assigned (must be of stream type)
   * @param expression The application expression
   * @param definitions The map with all definition expressions
   * @param currSrc The source listing where the generated code is attached to.
   * @return The modified source listing
   */
  def translateStreamDefinitionExpression(
    id: Identifier,
    expression: DefinitionExpression,
    definitions: Map[Identifier, DefinitionExpression],
    currSrc: CollectionType
  ): CollectionType = {
    expression match {
      case ApplicationExpression(TypeApplicationExpression(e, typeArgs, _), args, _) =>
        translateExternSignalExpression(id, externResolution(e, definitions), args, typeArgs, currSrc)
      case ApplicationExpression(e, args, _) =>
        translateExternSignalExpression(id, externResolution(e, definitions), args, Seq(), currSrc)
      case e =>
        throw Diagnostics
          .CoreASTError("Non valid stream defining expression cannot be translated", e.location)
    }
  }

  /**
   * Produces code for a x = nil expression
   *
   * @param id The id nil is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceNilStepCode(id: Identifier, ot: Type, currSrc: CollectionType): CollectionType

  /**
   * Produces code for a x = default(...) expression
   *
   * @param id The id default is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param stream The stream parameter of the default
   * @param defVal The default parameter of the default
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceDefaultStepCode(
    id: Identifier,
    ot: Type,
    stream: ExpressionArg,
    defVal: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = defaultFrom(...) expression
   *
   * @param id The id defaultFrom is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param stream The stream parameter of the defaultFrom
   * @param default The stream parameter of the defaultFrom
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceDefaultFromStepCode(
    id: Identifier,
    ot: Type,
    stream: ExpressionArg,
    default: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = time(...) expression
   *
   * @param id The id time is assigned to
   * @param stream The base-stream parameter of the time
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceTimeStepCode(id: Identifier, stream: ExpressionArg, currSrc: CollectionType): CollectionType

  /**
   * Produces code for a x = last(...) expression
   *
   * @param id The id last is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param values The value-stream parameter of the last
   * @param clock The trigger/clock-stream parameter of the last
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceLastStepCode(
    id: Identifier,
    ot: Type,
    values: ExpressionArg,
    clock: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = delay(...) expression
   *
   * @param id The id delay is assigned to
   * @param delay The delay-stream parameter of the delay
   * @param reset The reset-stream parameter of the delay
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceDelayStepCode(
    id: Identifier,
    delay: ExpressionArg,
    reset: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = lift(...) expression
   *
   * @param id The id lift is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param args The arguments of the lift except the last one (function)
   * @param function The lifted function
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceLiftStepCode(
    id: Identifier,
    ot: Type,
    args: Seq[ExpressionArg],
    function: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = slift(...) expression
   *
   * @param id The id slift is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param args The arguments of the lift except the last one (function)
   * @param function The lifted function
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceSignalLiftStepCode(
    id: Identifier,
    ot: Type,
    args: Seq[ExpressionArg],
    function: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = merge(...) expression
   *
   * @param id The id merge is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param args The stream expressions to be merged
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceMergeStepCode(
    id: Identifier,
    ot: Type,
    args: Seq[ExpressionArg],
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = count(...) expression
   *
   * @param id The id merge is assigned to
   * @param cntStream Expression of the stream to be counted
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceCountStepCode(id: Identifier, cntStream: ExpressionArg, currSrc: CollectionType): CollectionType

  /**
   * Produces code for a x = const(...) expression
   *
   * @param id The id merge is assigned to
   * @param ot Type of the output stream
   * @param value Argument expressions for const value
   * @param trigger Argument expression for triggering stream
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceConstStepCode(
    id: Identifier,
    ot: Type,
    value: ExpressionArg,
    trigger: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = filter(...) expression
   *
   * @param id The id merge is assigned to
   * @param ot Type of the output stream
   * @param value Argument expressions for filter value stream
   * @param condition Argument expressions for filter condition stream
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceFilterStepCode(
    id: Identifier,
    ot: Type,
    value: ExpressionArg,
    condition: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = fold(...) expression
   *
   * @param id The id merge is assigned to
   * @param ot Type of the output stream
   * @param stream Expression of the stream to be folded
   * @param init Expression of the initial value for the folding
   * @param function Expression of the function used for folding
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceFoldStepCode(
    id: Identifier,
    ot: Type,
    stream: ExpressionArg,
    init: ExpressionArg,
    function: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = reduce(...) expression
   *
   * @param id The id merge is assigned to
   * @param ot Type of the output stream
   * @param stream Expression of the stream to be reduced
   * @param function Expression of the function used for reducing
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceReduceStepCode(
    id: Identifier,
    ot: Type,
    stream: ExpressionArg,
    function: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = unitIf(...) expression
   *
   * @param id The id merge is assigned to
   * @param cond Expression of the stream with the condition for unitIf
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def produceUnitIfStepCode(
    id: Identifier,
    cond: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = pure(...) expression
   *
   * @param id The id merge is assigned to
   * @param ot Type of the output stream
   * @param valStream Stream to be filtered
   * @param currSrc The source listing the generated block is added to.
   * @return The modified source listing
   */
  def producePureStepCode(
    id: Identifier,
    ot: Type,
    valStream: ExpressionArg,
    currSrc: CollectionType
  ): CollectionType

  /**
   * Produces code for a x = native:&lt;name&gt;(...) expression
   *
   * @param id The id which is assigned (must be of stream type)
   * @param ot The Type of the output stream
   * @param name The function name which is applied with args and typeArgs and then assigned
   * @param args The arguments passed to e
   * @param typeArgs The type arguments passed to e
   * @param currSource The source listing where the generated code is attached to.
   * @return The modified source listing
   */
  def produceNativeFunctionStepCode(
    id: Identifier,
    ot: Type,
    name: String,
    args: Seq[ExpressionArg],
    typeArgs: Seq[Type],
    currSource: CollectionType
  ): CollectionType
}
