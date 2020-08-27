package de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend

import de.uni_luebeck.isp.tessla.core.{Diagnostic, Errors}
import de.uni_luebeck.isp.tessla.tessla_compiler.Diagnostics.CompilationWarning

import scala.reflect.internal.util.Position
import scala.reflect.internal.util.Position.formatMessage
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.FilteringReporter

/**
 * A [[scala.tools.nsc.reporters.Reporter]] implementation raising TeSSLa compiler errors if the supervised Scala compiler raises an error
 * and collects TeSSLa warnings if it raises warnings or information
 *
 * @param settings Settings passed to the extended [[FilteringReporter]]
 */
class TesslaCompilerReporter(val settings: Settings) extends FilteringReporter {

  val warnings: collection.mutable.ArrayBuffer[Diagnostic] = collection.mutable.ArrayBuffer()

  override def doReport(pos: Position, msg: String, severity: Severity): Unit = {
    val combMsg = formatMessage(pos, msg, shortenFile = false)
    println(combMsg)
    severity match {
      case reflect.internal.Reporter.INFO    => warnings += CompilationWarning(combMsg, "scalac", "info")
      case reflect.internal.Reporter.WARNING => warnings += CompilationWarning(combMsg, "scalac", "warning")
      case reflect.internal.Reporter.ERROR =>
        throw Errors.InternalError(s"Scala Compilation raised error, compilation aborted:\n$combMsg")
      case _ =>
        throw Errors.InternalError(
          s"Scala Compilation raised error of unknown Severity, compilation aborted:\n$combMsg"
        )
    }
  }
}