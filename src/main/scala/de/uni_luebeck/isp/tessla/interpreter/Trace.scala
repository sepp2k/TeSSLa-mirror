package de.uni_luebeck.isp.tessla.interpreter

import java.io.File

import de.uni_luebeck.isp.tessla.{Location, Tessla}
import de.uni_luebeck.isp.tessla.Errors.InternalError
import org.eclipse.tracecompass.ctf.core.trace.{CTFTrace, CTFTraceReader}

import scala.io.Source

object Trace {
  type Identifier = Tessla.Identifier
  val Identifier = Tessla.Identifier

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

  case class TimeStamp(loc: Location, time: Specification.Time) {
    override def toString: String = time.toString
  }

  def fromCtfFile(ctfFile: File, abortAt: Option[BigInt]): Interpreter.Trace = {
    new CtfEventIterator(new CTFTraceReader(new CTFTrace(ctfFile)), abortAt)
  }

  def fromLineIterator(
    lineIterator: Iterator[String],
    fileName: String,
    abortAt: Option[Specification.Time] = None
  ): Interpreter.Trace = {
    val rawTrace = TraceParser.parseTrace(lineIterator, fileName)
    new EventIterator(rawTrace, abortAt)
  }

  def fromSource(
    traceSource: Source,
    fileName: String,
    abortAt: Option[Specification.Time] = None
  ): Interpreter.Trace = {
    fromLineIterator(traceSource.getLines, fileName, abortAt)
  }

  def fromFile(file: File, abortAt: Option[Specification.Time] = None): Interpreter.Trace =
    fromSource(Source.fromFile(file), file.getName, abortAt)

  def fromString(string: String, fileName: String, abortAt: Option[Specification.Time] = None): Interpreter.Trace =
    fromSource(Source.fromString(string), fileName, abortAt)

  def fromCsvLineIterator(
    lineIterator: Iterator[String],
    fileName: String,
    abortAt: Option[Specification.Time] = None
  ): Interpreter.Trace = {
    val rawTrace = TraceParser.parseCsvTrace(lineIterator, fileName)
    new EventIterator(rawTrace, abortAt)
  }

  def fromCsvSource(
    traceSource: Source,
    fileName: String,
    abortAt: Option[Specification.Time] = None
  ): Interpreter.Trace = {
    fromCsvLineIterator(traceSource.getLines, fileName, abortAt)
  }

  def fromCsvFile(file: File, abortAt: Option[Specification.Time] = None): Interpreter.Trace =
    fromCsvSource(Source.fromFile(file), file.getName, abortAt)

  def fromCsvString(string: String, fileName: String, abortAt: Option[Specification.Time] = None): Interpreter.Trace =
    fromCsvSource(Source.fromString(string), fileName, abortAt)
}
