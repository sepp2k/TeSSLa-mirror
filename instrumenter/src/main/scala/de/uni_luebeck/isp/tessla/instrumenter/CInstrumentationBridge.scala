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

package de.uni_luebeck.isp.tessla.instrumenter

import scala.language.implicitConversions

import java.nio.file.Paths

import de.uni_luebeck.isp.clang_instrumentation.CPPBridge
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core._
import de.uni_luebeck.isp.tessla.instrumenter.CInstrumentation.{IFullFunDesc, IFunDesc}

/**
 * A bridge class, used to connect a library produced by the [[CInstrumentation]] to the native library provided by the
 * clang-instrumentation project.
 */
object CInstrumentationBridge {

  /**
   * The supported platforms of the clang-instrumentation library
   */
  // TODO: This should rather be in clang-instrumentation itself.
  val supportedPlatforms = Set(
    ("linux", "amd64"),
    ("windows", "amd64")
  )

  /**
   * Checks if the current platform is supported by the native library.
   * @return true if the platform is supported
   */
  def isPlatformSupported: Boolean = {
    val (os, arch) = (sys.props("os.name").toLowerCase(), sys.props("os.arch").toLowerCase())
    supportedPlatforms.exists {
      case (supportedOs, supportedArch) => supportedArch == arch && os.startsWith(supportedOs)
    }
  }

  /**
   * Define an [[IFullFunDesc]] by the given native function description.
   * @param f the native function description
   */
  class FullFunctionDesc(f: CPPBridge.FullFunctionDesc) extends FunctionDesc(f) with IFullFunDesc {
    override def parName(i: Int): String = f.parName(i)
  }

  /**
   * Define an [[IFunDesc]] by the given native function description
   * @param f the native function description
   */
  class FunctionDesc(f: CPPBridge.FunctionDesc) extends IFunDesc {
    override def name: String = f.name()

    override def retType: String = f.retType()

    override def parNum: Int = f.parNum()

    override def parType(i: Int): String = f.parType(i)
  }

  /**
   * Implicit conversion from a native to a non-native full function description
   */
  implicit def fullFunctionDescToIFullFunctionDesc(f: CPPBridge.FullFunctionDesc): IFullFunDesc =
    new FullFunctionDesc(f)

  /**
   * Implicit conversion from a native to a non-native function description
   */
  implicit def functionDescToIFunctionDesc(f: CPPBridge.FunctionDesc): IFunDesc = new FunctionDesc(f)

  /**
   * The instrumenter uses the [[CInstrumentation]] to generate a library interface, which it then binds to the
   * native library by extending the interface provided by the library, and delegating the calls. Finally, it then runs
   * the instrumentation process
   * @param cFileName the C file to instrument
   * @param inclPath the include paths to use
   */
  class Instrumenter(cFileName: String, inclPath: Seq[String] = Seq())
      extends TranslationPhase[Core.Specification, Unit] {

    private class Worker(iLib: CInstrumentation.ILibraryInterface) extends TranslationPhase.Translator[Unit] {
      override protected def translateSpec(): Unit = {
        if (!isPlatformSupported)
          throw Errors.InstrUnsupportedPlatform(supportedPlatforms)

        val libraryInterface: CPPBridge.LibraryInterface = new CPPBridge.LibraryInterface {

          override def checkInstrumentationRequiredFuncReturn(
            f: CPPBridge.FullFunctionDesc,
            filename: String,
            line: Int,
            col: Int
          ): String =
            iLib.checkInstFuncReturn(f, filename, line, col)

          override def checkInstrumentationRequiredFuncReturned(
            f: CPPBridge.FunctionDesc,
            containingFunc: CPPBridge.FullFunctionDesc,
            filename: String,
            line: Int,
            col: Int
          ): String =
            iLib.checkInstFuncReturned(f, containingFunc, filename, line, col)

          override def checkInstrumentationRequiredFuncCall(
            f: CPPBridge.FunctionDesc,
            containingFunc: CPPBridge.FullFunctionDesc,
            filename: String,
            line: Int,
            col: Int
          ): String =
            iLib.checkInstFuncCall(f, containingFunc, filename, line, col)

          override def checkInstrumentationRequiredFuncCalled(
            f: CPPBridge.FullFunctionDesc,
            filename: String,
            line: Int,
            col: Int
          ): String =
            iLib.checkInstFuncCalled(f, filename, line, col)

          override def checkInstrumentationRequiredWrite(
            pattern: String,
            `type`: String,
            containingFunc: CPPBridge.FullFunctionDesc,
            filename: String,
            line: Int,
            col: Int
          ): String =
            iLib.checkInstWrite(pattern, `type`, containingFunc, filename, line, col)

          override def checkInstrumentationRequiredRead(
            pattern: String,
            `type`: String,
            containingFunc: CPPBridge.FullFunctionDesc,
            filename: String,
            line: Int,
            col: Int
          ): String =
            iLib.checkInstRead(pattern, `type`, containingFunc, filename, line, col)

          override def getUserCbPrefix: String = iLib.getUserCbPrefix
          override def getCallbackCode(cbName: String): String = iLib.getCallbackCode(cbName)

          override def reportDiagnostic(`type`: String, message: String, file: String, line: Int, col: Int): Unit =
            iLib.reportDiagnostic(`type`, message, file, line, col)
        }

        val cFile = Paths.get(cFileName).toAbsolutePath
        inclPath.foreach(libraryInterface.addIncludePath)
        libraryInterface.runClang(cFile.getParent.toString, cFile.getFileName.toString)
      }

    }

    override def translate(spec: Core.Specification): TranslationPhase.Result[Unit] = {
      CInstrumentation.LibraryInterfaceFactory
        .translate(spec)
        .andThen(new Worker(_).translate())
    }

  }
}
