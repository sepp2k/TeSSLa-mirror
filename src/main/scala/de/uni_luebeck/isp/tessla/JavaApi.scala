package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}

import scala.collection.JavaConverters._

object JavaApi {
  case class Result(warnings: java.util.List[Diagnostic], errors: java.util.List[Diagnostic])

  def verify(tessla: String, fileName: String):Result = verify(tessla, fileName, null)

  def verify(tessla: String, fileName: String, timeUnit: String): Result = {
    val specSource = TesslaSource.fromString(tessla, path = fileName)
    val timeUnitSource = Option(timeUnit).map(TesslaSource.fromString(_, "timeunit"))
    val result = new Compiler().compile(specSource, timeUnitSource)
    result match {
      case Success(_, warnings) =>
        Result(warnings.asJava, List().asJava)
      case Failure(errors, warnings) =>
        Result(warnings.asJava, (errors: Seq[Diagnostic]).asJava)
    }
  }
}
