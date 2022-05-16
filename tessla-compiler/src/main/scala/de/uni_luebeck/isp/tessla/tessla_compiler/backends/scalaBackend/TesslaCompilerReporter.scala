/*
 * Copyright 2022 The TeSSLa Community
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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend

import de.uni_luebeck.isp.tessla.core.{Errors, Diagnostic as CoreDiagnostic}
import de.uni_luebeck.isp.tessla.tessla_compiler.Diagnostics.CompilationWarning
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.interfaces.Diagnostic.{ERROR, INFO, WARNING}
import dotty.tools.dotc.reporting.{AbstractReporter, Diagnostic}
import dotty.tools.dotc.util.SourcePosition

/**
 * A reporter implementation raising TeSSLa compiler errors if the supervised Scala compiler raises an error
 * and collects TeSSLa warnings if it raises warnings or information
 */
class TesslaCompilerReporter extends AbstractReporter {

  val warnings: collection.mutable.ArrayBuffer[CoreDiagnostic] = collection.mutable.ArrayBuffer()

  override def doReport(dia: Diagnostic)(using Context): Unit = {
    val combMsg = dia.pos.toString + dia.message
    println(combMsg)
    dia.level match {
      case INFO    => warnings += CompilationWarning(combMsg, "scalac", "info")
      case WARNING => warnings += CompilationWarning(combMsg, "scalac", "warning")
      case ERROR   => throw Errors.InternalError(s"Scala Compilation raised error, compilation aborted:\n$combMsg")
    }
  }
}
