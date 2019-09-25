package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}

import scala.collection.JavaConverters._
import de.uni_luebeck.isp.tessla
import de.uni_luebeck.isp.tessla.Errors.{DecreasingTimeStampsError, SameTimeStampError, TesslaError, TesslaErrorWithTimestamp}
import de.uni_luebeck.isp.tessla.interpreter._
import org.antlr.v4.runtime.CharStreams

import scala.collection.mutable

object JavaApi {
  case class Diagnostic(diagnostic: tessla.Diagnostic) {
    def message = diagnostic.message
    def fromLine = diagnostic.loc.range.map(_.fromLine).getOrElse(null)
    def fromColumn = diagnostic.loc.range.map(_.fromColumn).getOrElse(null)
    def toLine = diagnostic.loc.range.map(_.toLine).getOrElse(null)
    def toColumn = diagnostic.loc.range.map(_.toColumn).getOrElse(null)
    override def toString = diagnostic.toString
  }

  case class Result(warnings: java.util.List[Diagnostic], errors: java.util.List[Diagnostic])

  abstract class EngineListener {
    def event(stream: String, time: Specification.Time, value: TesslaCore.Value)

    def printEvent(time: Specification.Time, value: TesslaCore.Value)
  }

  case class CompilationResult(result: Result, engine: Engine)

  case class Engine(spec: Interpreter) {
    private val seen = mutable.Set.empty[String]

    def addListener(listener: EngineListener) {
      spec.outStreams.foreach {
        case (Some(name), stream, _) =>
          stream.addListener {
            case Some(value) =>
              listener.event(name, spec.getTime, value.forceValue)
            case None =>
          }
        case (None, stream, _) =>
          stream.addListener {
            case Some(value) =>
              listener.printEvent(spec.getTime, value.forceValue)
            case None =>
          }
      }
    }

    def provide(stream: String, value: Int): Boolean =
      provide(stream, TesslaCore.IntValue(BigInt(value), Location.unknown))

    def provide(stream: String, value: BigInt): Boolean =
      provide(stream, TesslaCore.IntValue(value, Location.unknown))

    def provide(stream: String, value: Boolean): Boolean =
      provide(stream, TesslaCore.BoolValue(value, Location.unknown))

    def provide(stream: String, value: String): Boolean =
      provide(stream, TesslaCore.StringValue(value, Location.unknown))

    def provide(stream: String): Boolean =
      provide(stream, TesslaCore.TesslaObject(Map(), Location.unknown))

    def provide(stream: String, value: TesslaCore.Value): Boolean =  {
      if (seen.contains(stream)) {
        throw SameTimeStampError(spec.getTime, stream, Location.unknown)
      }
      seen += stream

      spec.inStreams.get(stream) match {
        case Some((inStream, elementType)) =>
          ValueTypeChecker.check(value, elementType, stream)
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
    def setTime(time: Specification.Time): Unit = {
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

  def compile(tessla: String, fileName: String, timeUnit: String): CompilationResult = {
    val specSource = CharStreams.fromString(tessla, fileName)
    val compilerOptions = Compiler.Options(
      timeUnitString = Option(timeUnit),
      includeResolver = IncludeResolvers.empty,
      stdlibIncludeResolver = IncludeResolvers.fromStdlibResource,
      stdlibPath = "stdlib.tessla",
      currySignalLift = true
    )
    Compiler.compile(specSource, compilerOptions) match {
      case Success(spec, warnings) =>
        CompilationResult(Result(warnings.map(Diagnostic).asJava, List().asJava), Engine(new Interpreter(spec)))
      case Failure(errors, warnings) =>
        CompilationResult(Result(warnings.map(Diagnostic).asJava, errors.map(Diagnostic).asJava), null)
    }
  }
}
