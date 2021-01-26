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

package de.uni_luebeck.isp.tessla.interpreter

import java.io.File

import de.uni_luebeck.isp.tessla.core.Errors.InternalError
import de.uni_luebeck.isp.tessla.core.{Location, Tessla}
import org.eclipse.tracecompass.ctf.core.trace.{CTFTrace, CTFTraceReader}

import scala.io.Source

/**
 * Factory for [[Interpreter.Trace]]s.
 */

object Trace {
  type Identifier = Tessla.Identifier
  val Identifier = Tessla.Identifier

  /** Representation of a single event in a trace.
   *
    * @param loc the location information of this event, used for error messages
   * @param timeStamp the timestamp of the event
   * @param streamOpt the identifier of the stream this event is associated to. If not existing, the stream is
   *                  considered to be a raw stream.
   * @param value the data value of the event
   */
  case class Event(loc: Location, timeStamp: TimeStamp, streamOpt: Option[Identifier], value: Any) {
    override def toString: String = streamOpt match {
      case Some(stream) => s"$timeStamp: ${stream.name} = $value"
      case None         => value.toString
    }

    def stream: Identifier = streamOpt match {
      case Some(stream) => stream
      case None         => throw InternalError("Requested name of raw stream")
    }
  }

  case class TimeStamp(loc: Location, time: StreamEngine.Time) {
    override def toString: String = time.toString
  }

  def fromCtfFile(ctfFile: File, abortAt: Option[BigInt]): Interpreter.Trace = {
    new CtfEventIterator(new CTFTraceReader(new CTFTrace(ctfFile)), abortAt)
  }

  def fromLineIterator(
    lineIterator: Iterator[String],
    fileName: String,
    abortAt: Option[StreamEngine.Time] = None
  ): Interpreter.Trace = {
    val rawTrace = TraceParser.parseTrace(lineIterator, fileName)
    new EventIterator(rawTrace, abortAt)
  }

  def fromSource(
    traceSource: Source,
    fileName: String,
    abortAt: Option[StreamEngine.Time] = None
  ): Interpreter.Trace = {
    fromLineIterator(traceSource.getLines(), fileName, abortAt)
  }

  def fromFile(file: File, abortAt: Option[StreamEngine.Time] = None): Interpreter.Trace =
    fromSource(Source.fromFile(file), file.getName, abortAt)

  def fromString(string: String, fileName: String, abortAt: Option[StreamEngine.Time] = None): Interpreter.Trace =
    fromSource(Source.fromString(string), fileName, abortAt)

  def fromCsvLineIterator(
    lineIterator: Iterator[String],
    fileName: String,
    abortAt: Option[StreamEngine.Time] = None
  ): Interpreter.Trace = {
    val rawTrace = TraceParser.parseCsvTrace(lineIterator, fileName)
    new EventIterator(rawTrace, abortAt)
  }

  def fromCsvSource(
    traceSource: Source,
    fileName: String,
    abortAt: Option[StreamEngine.Time] = None
  ): Interpreter.Trace = {
    fromCsvLineIterator(traceSource.getLines(), fileName, abortAt)
  }

  def fromCsvFile(file: File, abortAt: Option[StreamEngine.Time] = None): Interpreter.Trace =
    fromCsvSource(Source.fromFile(file), file.getName, abortAt)

  def fromCsvString(string: String, fileName: String, abortAt: Option[StreamEngine.Time] = None): Interpreter.Trace =
    fromCsvSource(Source.fromString(string), fileName, abortAt)
}
