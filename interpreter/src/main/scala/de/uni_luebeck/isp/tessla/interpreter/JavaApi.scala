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

package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.core
import de.uni_luebeck.isp.tessla.core.Errors._
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Failure, Success}
import de.uni_luebeck.isp.tessla.core.{Compiler, ConstantEvaluator, IncludeResolvers, Location, TesslaAST}
import org.antlr.v4.runtime.{CharStream, CharStreams}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object JavaApi {

  case class Diagnostic(diagnostic: core.Diagnostic) {
    def message = diagnostic.message

    def fromLine = diagnostic.loc.range.map(_.fromLine).getOrElse(null)

    def fromColumn = diagnostic.loc.range.map(_.fromColumn).getOrElse(null)

    def toLine = diagnostic.loc.range.map(_.toLine).getOrElse(null)

    def toColumn = diagnostic.loc.range.map(_.toColumn).getOrElse(null)

    override def toString = diagnostic.toString
  }

  case class Result(warnings: java.util.List[Diagnostic], errors: java.util.List[Diagnostic])

  abstract class EngineListener {
    def event(stream: String, time: StreamEngine.Time, value: Any): Unit

    def printEvent(time: StreamEngine.Time, value: Any): Unit
  }

  case class CompilationResult(result: Result, engine: Engine)

  case class Engine(spec: Interpreter) {
    private val seen = mutable.Set.empty[String]

    def addListener(listener: EngineListener): Unit = {
      spec.outStreams.foreach {
        case (Some(name), stream, _) =>
          stream.addListener {
            case Some(value) =>
              listener.event(name, spec.getTime, value)
            case None =>
          }
        case (None, stream, _) =>
          stream.addListener {
            case Some(value) =>
              listener.printEvent(spec.getTime, value)
            case None =>
          }
      }
    }

    def provide(stream: String, value: Int): Boolean =
      provide(stream, BigInt(value))

    def provide(stream: String, value: BigInt): Boolean =
      provide(stream, value)

    def provide(stream: String, value: Boolean): Boolean =
      provide(stream, value)

    def provide(stream: String, value: String): Boolean =
      provide(stream, value)

    def provide(stream: String): Boolean =
      provide(stream, ConstantEvaluator.Record(Map()))

    def provide(stream: String, value: Any): Boolean = {
      if (seen.contains(stream)) {
        throw SameTimeStampError(spec.getTime, stream, Location.unknown)
      }
      seen += stream

      spec.inStreams.get(stream) match {
        case Some((inStream, elementType)) =>
          val tpe = elementType.asInstanceOf[TesslaAST.Core.InstantiatedType]
          assert(tpe.name == "Events")
          assert(tpe.typeArgs.size == 1)
          RuntimeTypeChecker
            .check(tpe.typeArgs.head, value)
            .foreach(error => throw mkTesslaError("input " + stream + ": " + error))
          inStream.provide(value)
          true
        case None =>
          // ignore undeclared input streams
          false
      }
    }

    /**
     * Propagates all inputs and progresses time to the given timestamp.
     */
    def setTime(time: Int): Unit = setTime(BigInt(time))

    /**
     * Propagates all inputs and progresses time to the given timestamp.
     */
    def setTime(time: StreamEngine.Time): Unit = {
      if (time > spec.getTime) {
        try {
          spec.step(time - spec.getTime)
          seen.clear()
        } catch {
          case err: TesslaError => throw TesslaErrorWithTimestamp(err, spec.getTime)
        }
      } else if (time < spec.getTime) {
        throw DecreasingTimeStampsError(spec.getTime, time, Location.unknown)
      }
    }

    /**
     * Propagates all inputs without progressing time.
     * Can only be called once per point in time.
     * No more input values can be provided for the current time afterwards.
     */
    def step(): Unit = {
      try {
        spec.step()
      } catch {
        case err: TesslaError => throw TesslaErrorWithTimestamp(err, spec.getTime)
      }
    }
  }

  def verify(tessla: String, fileName: String): Result =
    compile(tessla, fileName).result

  def verify(tessla: String, fileName: String, timeUnit: String) =
    compile(tessla, fileName, timeUnit).result

  def compile(tessla: String, fileName: String): CompilationResult =
    compile(tessla, fileName, null)

  def compile(tessla: String, fileName: String, baseTime: String): CompilationResult = {
    val specSource = CharStreams.fromString(tessla, fileName)
    val compilerOptions = Compiler.Options(
      baseTimeString = Option(baseTime),
      includeResolver = IncludeResolvers.empty,
      stdlibIncludeResolver = IncludeResolvers.fromStdlibResource,
      stdlibPath = "stdlib.tessla",
      flattenCore = false
    )
    compile(specSource, compilerOptions)
  }

  def compile(specSource: CharStream, compilerOptions: Compiler.Options): CompilationResult = {
    Compiler.compile(specSource, compilerOptions) match {
      case Success(spec, warnings) =>
        CompilationResult(
          Result(warnings.map(Diagnostic).asJava, List().asJava),
          Engine(new Interpreter(spec))
        )
      case Failure(errors, warnings) =>
        CompilationResult(
          Result(warnings.map(Diagnostic).asJava, errors.map(Diagnostic).asJava),
          null
        )
    }
  }
}
