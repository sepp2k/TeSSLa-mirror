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
import scala.io.Source

private case class SourceSegments(
  variables: StringBuilder = new StringBuilder,
  input: StringBuilder = new StringBuilder,
  timestamp: StringBuilder = new StringBuilder,
  computation: StringBuilder = new StringBuilder,
  output: StringBuilder = new StringBuilder
)

/**
 * Class implementing TranslationPhase for the translation from TeSSLa Core to
 * rust code
 * The translation of stream functions is performed in StreamCodeGenerator TODO
 * The translation of other expressions in NonStreamCodeGenerator TODO
 * @param ioInterface Indicates whether the generated code shall be able to read/write from/to stdio
 */
class TesslaCoreToRust(ioInterface: Boolean) extends TranslationPhase[ExtendedSpecification, String] {
  private val sourceTemplate: String =
    if (ioInterface) "de/uni_luebeck/isp/tessla/tessla_compiler/RustSkeleton.rs"
    else "de/uni_luebeck/isp/tessla/tessla_compiler/RustSkeletonNoIO.rs"

  override def translate(extSpec: ExtendedSpecification): Result[String] =
    new Translator(extSpec).translate()

  class Translator(extSpec: ExtendedSpecification) extends TranslationPhase.Translator[String] {

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
              definition match {
                case ApplicationExpression(TypeApplicationExpression(e, typeArgs, _), args, _) =>
                  translateExternSignalExpression(id, externResolution(e), args, typeArgs, srcSegments)
                case ApplicationExpression(e, args, _) =>
                  translateExternSignalExpression(id, externResolution(e), args, Seq(), srcSegments)
                case e =>
                  throw Diagnostics
                    .CoreASTError("Non valid stream defining expression cannot be translated", e.location)
              }
            case e =>
              throw Diagnostics
                .NotYetImplementedError("Failed to translate an expression without instantiated type", e.location)
          }
      }

      // Produce output generation
      extSpec.spec.out.foreach { o =>
        val name = TesslaAST.Core.getOutputName(o._2)
        produceOutputCode(o._1.id, getStreamType(o._1.id), name, srcSegments, o._2.contains("raw"))
      }

      // Produce input consumption
      extSpec.spec.in.foreach { i =>
        produceInputCode(i._1, i._2._1, srcSegments)
      }

      insertSegments(srcSegments)
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
     * Translates an assignment to a stream variable to the corresponding rust function and appends it to the
     * correct source segment. The assigned expression has to be (and is always in TeSSLa Core) an application.
     *
     * @param id The id which is assigned (must be of stream type)
     * @param e The expression which is applied with args and typeArgs and then assigned
     * @param args The arguments passed to e
     * @param typeArgs The type arguments passed to e
     * @param srcSegments The source segments where the generated code is attached to. It is attached to the
     *                   calculations section of the source.
     */
    def translateExternSignalExpression(
      id: Identifier,
      e: ExternExpression,
      args: Seq[ExpressionArg],
      typeArgs: Seq[Type],
      srcSegments: SourceSegments
    ): Unit = {
      e.name match {
        //case "nil" => TODO does this do anything ??
        case "default" =>
          // TODO args(1) is the "default" value, do we need this elsewhere?
          val s = streamNameFromExpressionArg(args.head)
          val o = s"var_${id.fullName}"
          srcSegments.computation.append(s"default(${o}, ${s})")
        /*case "defaultFrom" =>
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
          produceConstStepCode(id, typ.resultType.resolve(typeParamMap), args, currSource)
        case "filter" =>
          produceFilterStepCode(id, typ.resultType.resolve(typeParamMap), args, currSource)
        case "fold" =>
          produceFoldStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), args(2), currSource)
        case "reduce" =>
          produceReduceStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
        case "unitIf" =>
          produceUnitIfStepCode(id, args(0), currSource)
        case "pure" =>
          producePureStepCode(id, typ.resultType.resolve(typeParamMap), args(0), currSource)*/
        case _ => throw Diagnostics.CommandNotSupportedError(e.toString)
      }
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

      if (ioInterface) {
        throw Diagnostics
          .NotYetImplementedError("Output via IOInterface")
      } else {
        srcSegments.output.append(s"""output_stream($s, "$name", currTs, $raw)""")
      }
    }

    /**
     * Produces code reading the input (variable inputStream and value) and storing it.
     * For parsing the input string to an exact value a function __[TC]inputParse__ is called
     * which has to be translated in the following phases.
     *
     * @param inStream The input stream to be handled
     * @param typ      The input stream's type. Must be Events[...].
     * @param srcSegments  The source segments the generated block is added to.
     *                 There is code attached to the input, and output section.
     */
    def produceInputCode(inStream: Identifier, typ: Type, srcSegments: SourceSegments): Unit = {
      val s = s"var_${inStream.fullName}"

      srcSegments.input.append(s"if inputStream == ${inStream.idOrName.left.get} { get_value_from_input($s) }")

      srcSegments.output.append(s"update_last($s)")
    }
  }
}
