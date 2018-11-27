package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}

import scala.collection.JavaConverters._
import de.uni_luebeck.isp.tessla

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

  def verify(tessla: String, fileName: String):Result = verify(tessla, fileName, null)

  def verify(tessla: String, fileName: String, timeUnit: String): Result = {
    val specSource = TesslaSource.fromString(tessla, path = fileName)
    val timeUnitSource = Option(timeUnit).map(TesslaSource.fromString(_, "timeunit"))
    val result = new Compiler().compile(specSource, timeUnitSource)
    result match {
      case Success(_, warnings) =>
        Result(warnings.map(Diagnostic(_)).asJava, List().asJava)
      case Failure(errors, warnings) =>
        Result(warnings.map(Diagnostic(_)).asJava, errors.map(Diagnostic(_)).asJava)
    }
  }
}
