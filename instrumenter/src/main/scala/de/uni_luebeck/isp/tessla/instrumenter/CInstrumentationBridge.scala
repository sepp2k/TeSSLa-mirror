/*
 * Copyright (c) 2020 Institute of Software Engineering and Programming Languages,
 * University of Lübeck, Germany
 *
 * Modified MIT license:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this binary (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software and the code which is
 * generated by the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.uni_luebeck.isp.tessla.instrumenter

import java.nio.file.Paths

import de.uni_luebeck.isp.clang_instrumentation.CPPBridge
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core._
import de.uni_luebeck.isp.tessla.instrumenter.CInstrumentation.{IFullFunDesc, IFunDesc}

object CInstrumentationBridge {
  val supportedPlatforms = Set(
    ("linux", "amd64"),
    ("windows", "amd64")
  )

  def isPlatformSupported: Boolean = {
    val (os, arch) = (sys.props("os.name").toLowerCase(), sys.props("os.arch").toLowerCase())
    supportedPlatforms.exists {
      case (supportedOs, supportedArch) => supportedArch == arch && os.startsWith(supportedOs)
    }
  }

  class FullFunctionDesc(f: CPPBridge.FullFunctionDesc) extends FunctionDesc(f) with IFullFunDesc {
    override def parName(i: Int): String = f.parName(i)
  }

  class FunctionDesc(f: CPPBridge.FunctionDesc) extends IFunDesc {
    override def name(): String = f.name()

    override def name(name: String): Unit = f.name(name)

    override def retType(): String = f.retType()

    override def retType(retType: String): Unit = f.retType(retType)

    override def parNum(): Int = f.parNum()

    override def parNum(parNum: Int): Unit = f.parNum(parNum)

    override def parType(i: Int): String = f.parType(i)
  }

  implicit def fullFunctionDescToIFullFunctionDesc(f: CPPBridge.FullFunctionDesc): IFullFunDesc =
    new FullFunctionDesc(f)
  implicit def functionDescToIFunctionDesc(f: CPPBridge.FunctionDesc): IFunDesc =
    new FunctionDesc(f)

  class Instrumenter(cFileName: String, inclPath: Seq[String] = Seq())
      extends TranslationPhase[Core.Specification, Unit] {

    override def translate(spec: Core.Specification): TranslationPhase.Result[Unit] = {
      new CInstrumentation.LibraryInterfaceFactory(spec).translate().map { iLib =>
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

  }
}
