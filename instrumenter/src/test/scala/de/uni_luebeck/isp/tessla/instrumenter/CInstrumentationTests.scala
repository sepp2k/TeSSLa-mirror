package de.uni_luebeck.isp.tessla.instrumenter

import de.uni_luebeck.isp.tessla.TestCase.{PathResolver, TestConfig}
import de.uni_luebeck.isp.tessla.{AbstractTestRunner, TestCase}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.instrumenter.CInstrumentation.{IFullFunDesc, IFunDesc, ILibraryInterface}

class CInstrumentationTests extends AbstractTestRunner[ILibraryInterface]("CInstrumentation") {

  override def roots: Seq[String] = Seq("instrumenter/")

  override def translation: TranslationPhase[Core.Specification, ILibraryInterface] =
    CInstrumentation.LibraryInterfaceFactory

  val Line = """^([A-Z]+)\s+(.*)$""".r
  val Function = """^([A-z0-9_]+):\s+\(((?:.+(?:,\s+.+)*)?)\)\s+=>\s+(.+)$""".r
  val Pattern = """(.*)\s+(.*)""".r

  class FullFunctionDesc(name: String, parameters: Vector[(String, String)], returnType: String)
      extends FunctionDesc(name, parameters.map(_._2), returnType)
      with IFullFunDesc {
    override def parName(i: Int): String = parameters(i)._1
  }

  class FunctionDesc(id: String, parameters: Vector[String], returnType: String) extends IFunDesc {
    override def name(): String = id

    override def name(name: String): Unit = ???

    override def retType(): String = returnType

    override def retType(retType: String): Unit = ???

    override def parNum(): Int = parameters.size

    override def parNum(parNum: Int): Unit = ???

    override def parType(i: Int): String = parameters(i)
  }

  override def run(
    spec: ILibraryInterface,
    inputFile: String,
    testCase: TestConfig,
    resolver: PathResolver
  ): (String, String) = {
    val callbacks = resolver.source(inputFile) { in =>
      in.getLines()
        .map {
          case line @ Line(mode, value) =>
            lazy val function = value match {
              case Function(name, args, ret) =>
                val a = args.split("""\s*,\s*""").toVector.zipWithIndex.map {
                  case (t, id) => (s"arg$id", t)
                }
                new FullFunctionDesc(name, a, ret)
              case _ => fail(s"Failed to parse function $value")
            }
            lazy val (typ, pat) = value match {
              case Pattern(typ, pat) => (typ, pat)
              case _                 => fail(s"Failed to parse pattern $value")
            }

            val file = resolver.resolve(inputFile)
            line -> (mode match {
              case "CALL"     => spec.checkInstFuncCall(function, null, file, 0, 0)
              case "CALLED"   => spec.checkInstFuncCalled(function, file, 0, 0)
              case "RETURN"   => spec.checkInstFuncReturn(function, file, 0, 0)
              case "RETURNED" => spec.checkInstFuncReturned(function, null, file, 0, 0)
              case "READ"     => spec.checkInstRead(pat, typ, null, file, 0, 0)
              case "WRITE"    => spec.checkInstWrite(pat, typ, null, file, 0, 0)
            })

        }
        .filterNot(_._2.isBlank)
        .toList
    }

    val output = callbacks
      .map {
        case (line, cbName) =>
          s"""$line
           |$cbName
           |${spec.getCallbackCode(cbName)}""".stripMargin
      }
      .mkString("\n")

    (output, "")
  }

  override def compareRunResult(
    actualOut: String,
    actualErr: String,
    expectedOut: String,
    expectedErr: String
  ): Unit = {
    assert(actualErr === expectedErr)
    assert(actualOut === expectedOut)
  }
}
