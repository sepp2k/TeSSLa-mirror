package de.uni_luebeck.isp.tessla.analyses

import java.nio.file.Paths

import de.uni_luebeck.isp.tessla.core.{CPatternLexer, CPatternParser, Errors, Location, TranslationPhase}
import de.uni_luebeck.isp.clang_instrumentation.CPPBridge
import de.uni_luebeck.isp.tessla.core.Errors.{InternalError, ParserError}
import de.uni_luebeck.isp.tessla.core.CPatternParser.{
  ArrayAccessContext,
  DereferenceContext,
  ParenthesesContext,
  PatternContext,
  ReferenceContext,
  StructUnionAccessArrowContext,
  StructUnionAccessContext,
  VariableContext
}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Failure
import org.antlr.v4.runtime.{BaseErrorListener, CharStreams, CommonTokenStream, RecognitionException, Recognizer, Token}
import de.uni_luebeck.isp.tessla.core.util._

import scala.collection.mutable
import scala.util.Try

object Observations {
  sealed abstract class Pattern
  final case class Variable(functionName: Option[String], name: String) extends Pattern {
    override def toString: String = functionName match {
      case Some(value) => s"$value::$name"
      case None        => name
    }
  }
  final case class StructUnionAccess(base: Pattern, fieldName: String, isArrow: Boolean = false) extends Pattern {
    override def toString: String = if (isArrow) {
      s"$base->$fieldName"
    } else {
      s"$base.$fieldName"
    }
  }
  final case class ArrayAccess(base: Pattern) extends Pattern {
    override def toString: String = s"$base[]"
  }
  final case class Reference(base: Pattern) extends Pattern {
    override def toString: String = s"&($base)"
  }
  final case class Dereference(base: Pattern) extends Pattern {
    override def toString: String = s"*($base)"
  }

  class InstrumenterWorker(spec: Core.Specification, cFileName: String) extends TranslationPhase.Translator[Unit] {

    def parsePattern(str: String, loc: Location): Pattern = {
      val src = CharStreams.fromString(str, loc.path)
      val lexer = new CPatternLexer(src)
      lexer.setLine(loc.range.map(_.fromLine).getOrElse(0))
      lexer.setCharPositionInLine(loc.range.map(_.fromColumn).getOrElse(0))
      val tokens = new CommonTokenStream(lexer)
      val parser = new CPatternParser(tokens)
      parser.removeErrorListeners()
      parser.addErrorListener(new BaseErrorListener {
        override def syntaxError(
          r: Recognizer[_, _],
          offendingToken: Any,
          l: Int,
          c: Int,
          msg: String,
          e: RecognitionException
        ) = {
          error(ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token])))
        }
      })

      val pattern = parser.start().pattern()

      if (parser.getNumberOfSyntaxErrors > 0) {
        abortOnError()
      }

      def translatePattern(context: PatternContext): Pattern = context match {
        case ctx: ArrayAccessContext => ArrayAccess(translatePattern(ctx.pattern()))
        case ctx: ReferenceContext   => Reference(translatePattern(ctx.pattern()))
        case ctx: VariableContext    => Variable(Option(ctx.functionName).map(_.getText), ctx.name.getText)
        case ctx: StructUnionAccessContext =>
          StructUnionAccess(translatePattern(ctx.pattern()), ctx.ID().getSymbol.getText)
        case ctx: StructUnionAccessArrowContext =>
          StructUnionAccess(translatePattern(ctx.pattern()), ctx.ID().getSymbol.getText, isArrow = true)
        case ctx: DereferenceContext => Dereference(translatePattern(ctx.pattern()))
        case ctx: ParenthesesContext => translatePattern(ctx.pattern())
      }

      translatePattern(pattern)
    }

    type Annotation = (String, Core.ExpressionArg)
    type InStream = (Core.Identifier, Core.Type)
    def noArgError(name: String, loc: Location): InternalError =
      InternalError(
        "Annotation has no argument " + name + ", should have been caught by the (not yet implemented) type checker.",
        loc
      )

    private def argumentAsString(annotation: Annotation, argumentName: String): String = {
      val loc = annotation._2.location
      val argument = annotationArgs(annotation).getOrElse(argumentName, throw noArgError(argumentName, loc))

      argument match {
        case s: Core.StringLiteralExpression => s.value
        case _ =>
          throw InternalError(
            "Expression must be a string, should have been caught by the (not yet implemented) type checker.",
            argument.location
          )
      }
    }

    private def annotationArgs(annotation: Annotation): Map[String, Core.ExpressionArg] =
      annotation._2 match {
        case r: Core.RecordConstructorExpression => r.entries.mapVals(_._1)
        case _                                   => Map()
      }

    private def argumentAsInt(annotation: Annotation, argumentName: String): Int = {
      val loc = annotation._2.location
      val argument = annotationArgs(annotation).getOrElse(argumentName, throw noArgError(argumentName, loc))

      argument match {
        case s: Core.IntLiteralExpression => s.value.intValue
        case _ =>
          throw InternalError(
            "Expression must be a string, should have been caught by the (not yet implemented) type checker.",
            argument.location
          )
      }
    }

    val threadIdInStreams = spec.in.filter {
      case (_, (_, annotations)) => annotations.keySet.contains("ThreadId")
    }

    protected def encloseInstrumentationCode(code: String): String = {
      val lines = code.split("\n") ++
        threadIdInStreams.map { in => s"""trace_push_thread_id(events, "${in._1.fullName}");""" }
      s"uint8_t* events = trace_create_events(${lines.length});\n" +
        lines.sorted.mkString("\n") +
        "\ntrace_write(events);"
    }

    private def mergeObservations(observations: Seq[(String, String)]*) =
      observations.flatten.groupBy(_._1).map {
        case (name, seq) =>
          val code = seq.map(_._2).mkString("\n")
          name -> encloseInstrumentationCode(code)
      }

    private def merge[A](maps: Map[String, Seq[A]]*): Map[String, Seq[A]] = maps.flatten.groupBy(_._1).map {
      case (key, seq) => key -> seq.flatMap(_._2)
    }

    private def createFunctionObservations(annotationName: String): Map[String, String] =
      mergeObservations(
        createFunctionObservations(annotationName, (_, in) => printEventValue(in)),
        createFunctionObservations(
          annotationName + "Arg",
          (annotation, in) => printEventArgument(in, argumentAsInt(annotation, "index"))
        )
      )

    private def createFunctionObservations(
      annotationName: String,
      createCode: (Annotation, InStream) => String
    ): Seq[(String, String)] =
      spec.in.toSeq.flatMap {
        case (id, (tpe, annotations)) =>
          annotations
            .filter(_._1 == annotationName)
            .map {
              case (name, args) => (name, args.head)
            }
            .map { annotation =>
              val name = argumentAsString(annotation, "name")
              val code = createCode(annotation, (id, tpe))
              (name, code)
            }
      }

    private def unwrapType(streamType: Core.Type) = streamType match {
      case Core.InstantiatedType("Events", t :: Nil, _) => t
      case _                                            => throw Errors.InternalError(s"Failed to unwrap type $streamType.")
    }

    private def assertUnit(annotationName: String): Unit =
      spec.in.foreach {
        case (id, (streamType, annotations)) =>
          annotations.filter(_._1 == annotationName).foreach { annotation =>
            unwrapType(streamType) match {
              case Core.UnitType => //good
              case _             => error(Errors.WrongType("Events[Unit]", streamType, id.location))
            }
          }
      }

    private def createFunctionTypeAssertions(annotationName: String): Map[String, Seq[(Int, InStream)]] =
      spec.in
        .flatMap {
          case (id, (streamType, annotations)) =>
            annotations
              .filter(_._1 == annotationName)
              .map {
                case (name, args) => (name, args.head)
              }
              .map { annotation =>
                val name = argumentAsString(annotation, "name")
                val argIndex = Try(argumentAsInt(annotation, "index")) getOrElse -1
                name -> (argIndex -> (id, streamType))
              }
        }
        .groupBy(_._1)
        .mapVals(_.values.toSeq)

    private def createPatternObservations(
      annotationName: String,
      createCode: (Annotation, InStream) => String
    ): Seq[(Pattern, Option[String], String, InStream)] =
      spec.in.toSeq.flatMap {
        case (id, (streamType, annotations)) =>
          annotations
            .filter(_._1 == annotationName)
            .toSeq
            .map {
              case (name, args) => (name, args.head)
            }
            .map { annotation =>
              val function = Try(argumentAsString(annotation, "function")) toOption
              val lvalue = argumentAsString(annotation, "lvalue")
              val pattern = parsePattern(lvalue, annotationArgs(annotation)("lvalue").location)
              val code = createCode(annotation, (id, streamType))
              (pattern, function, code, (id, streamType))
            }
      }

    protected def printEvent(in: InStream, value: String): String = {
      val name = in._1.fullName

      unwrapType(in._2) match {
        case Core.IntType =>
          s"""trace_push_int(events, "$name", (int64_t) $value);"""
        case Core.FloatType =>
          s"""trace_push_float(events, "$name", (double) $value);"""
        case Core.BoolType =>
          s"""trace_push_bool(events, "$name", (bool) $value);"""
        case Core.UnitType =>
          s"""trace_push_unit(events, "$name");"""
        case _ => ""
      }
    }

    protected def printEventValue(in: InStream): String = printEvent(in, "value")

    protected def printEventArgument(in: InStream, index: Int): String = printEvent(in, s"arg$index")

    protected def printEventIndex(in: InStream, index: Int): String = printEvent(in, s"index$index")

    protected val prefix = "#include \"logging.h\"\n"

    val alreadyTypeAssertedStreams = mutable.Set.empty[InStream]

    protected def assertSameType(cType: String, stream: InStream): Unit = {
      if (alreadyTypeAssertedStreams(stream)) return;

      alreadyTypeAssertedStreams.add(stream)

      val types = Map(
        "Int" -> List(
          "int",
          "byte",
          "short",
          "long",
          "long long",
          "char",
          "int64_t",
          "int32_t",
          "int16_t",
          "int8_t",
          "uint64_t",
          "uint32_t",
          "uint16_t",
          "uint8_t"
        ),
        "Bool" -> List("bool", "_Bool"),
        "Float" -> List("float", "double"),
        "()" -> List("void")
      )
      val (id, tpe) = stream
      val streamName = id.fullName
      unwrapType(tpe) match {
        case Core.UnitType =>
          if (cType != "void") {
            warn(id.location, s"Stream $streamName was declared as $tpe, but mapped to $cType.")
          }
        case t: Core.InstantiatedType if types.contains(t.name) =>
          if (!(types(t.name).contains(cType) || cType.endsWith("*") && t.name == "Int")) {
            warn(id.location, s"Stream $streamName was declared as $tpe, but mapped to $cType.")
          }
        case _ =>
          val expectedType = if (cType.endsWith("*")) {
            "Events[Int]"
          } else {
            types.collect {
              case (streamType, cTypes) if cTypes.contains(cType) => streamType
            } match {
              case head :: _ => s"Events[$head]"
              case Nil       => "Events[Int], Events[Float] or Events[Bool]"
            }
          }
          error(Errors.WrongType(expectedType, tpe, id.location))
      }
    }

    override protected def translateSpec(): Unit = {
      val functionCall = createFunctionObservations("InstFunctionCall")
      assertUnit("InstFunctionCall")
      val functionCallTypeAssertions = createFunctionTypeAssertions("InstFunctionCallArg")

      val functionCalled = createFunctionObservations("InstFunctionCalled")
      assertUnit("InstFunctionCalled")
      val functionCalledTypeAssertions = createFunctionTypeAssertions("InstFunctionCalledArg")

      val functionReturn = createFunctionObservations("InstFunctionReturn")
      val functionReturnTypeAssertions =
        merge(createFunctionTypeAssertions("InstFunctionReturn"), createFunctionTypeAssertions("InstFunctionReturnArg"))

      val functionReturned = createFunctionObservations("InstFunctionReturned")
      val functionReturnedTypeAssertions = merge(
        createFunctionTypeAssertions("InstFunctionReturned"),
        createFunctionTypeAssertions("InstFunctionReturnedArg")
      )

      // TODO merge pattern observations for same pattern into one observation entry and call encloseInstrumentationCode

      val writes = createPatternObservations("InstWrite", (annotation, in) => printEventValue(in)) ++
        createPatternObservations(
          "InstWriteIndex",
          (annotation, in) => printEventIndex(in, argumentAsInt(annotation, "index"))
        )

      val writesInFunction =
        createPatternObservations("InstWriteInFunction", (annotation, in) => printEventValue(in)) ++
          createPatternObservations(
            "InstWriteInFunctionIndex",
            (annotation, in) => printEventIndex(in, argumentAsInt(annotation, "index"))
          )

      val reads = createPatternObservations("InstRead", (annotation, in) => printEventValue(in)) ++
        createPatternObservations(
          "InstReadIndex",
          (annotation, in) => printEventIndex(in, argumentAsInt(annotation, "index"))
        )

      val readsInFunction = createPatternObservations("InstReadInFunction", (annotation, in) => printEventValue(in)) ++
        createPatternObservations(
          "InstReadInFunctionIndex",
          (annotation, in) => printEventIndex(in, argumentAsInt(annotation, "index"))
        )

      val callbacks = functionCall.map {
        case (name, code) =>
          name + "_call" -> code
      } ++ functionCalled.map {
        case (name, code) =>
          name + "_called" -> code
      } ++ functionReturn.map {
        case (name, code) =>
          name + "_return" -> code
      } ++ functionReturned.map {
        case (name, code) =>
          name + "_returned" -> code
      }

      val libraryInterface: CPPBridge.LibraryInterface = new CPPBridge.LibraryInterface {
        def assertTypes(f: CPPBridge.FunctionDesc, assertions: Map[String, Seq[(Int, InStream)]]): Unit = {
          assertions.get(f.name).foreach { seq =>
            seq.foreach {
              case (argIndex, inStream) =>
                if (argIndex >= f.parNum()) {
                  error(Errors.InstArgDoesNotExist(f.name, argIndex, inStream._1.location))
                } else if (argIndex < 0) {
                  assertSameType(f.retType(), inStream)
                } else {
                  assertSameType(f.parType(argIndex), inStream)
                }
            }
          }
        }

        override def checkInstrumentationRequiredFuncReturn(
          f: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String = {
          assertTypes(f, functionReturnTypeAssertions)
          if (functionReturn.contains(f.name)) {
            f.name + "_return"
          } else {
            ""
          }
        }

        override def checkInstrumentationRequiredFuncReturned(
          f: CPPBridge.FunctionDesc,
          containingFunc: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String = {
          assertTypes(f, functionReturnedTypeAssertions)
          if (functionReturned.contains(f.name)) {
            f.name + "_returned"
          } else {
            ""
          }
        }

        override def checkInstrumentationRequiredFuncCall(
          f: CPPBridge.FunctionDesc,
          containingFunc: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String = {
          assertTypes(f, functionCallTypeAssertions)
          if (functionCall.contains(f.name)) {
            f.name + "_call"
          } else {
            ""
          }
        }

        override def checkInstrumentationRequiredFuncCalled(
          f: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String = {
          assertTypes(f, functionCalledTypeAssertions)
          if (functionCalled.contains(f.name)) {
            f.name + "_called"
          } else {
            ""
          }
        }

        override def checkInstrumentationRequiredWrite(
          pattern: String,
          typ: String,
          containingFunc: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String = {
          // TODO assert types and create callbacks for writes and writesInFunction
          ""
        }

        override def checkInstrumentationRequiredRead(
          pattern: String,
          typ: String,
          containingFunc: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String = {
          // TODO assert types and create callbacks for reads and readsInFunction
          ""
        }

        override def getUserCbPrefix: String = prefix

        override def getCallbackCode(cbName: String): String = {
          callbacks.getOrElse(cbName, "")
        }

        override def reportDiagnostic(typ: String, message: String, file: String, line: Int, col: Int): Unit = {
          System.err.println(s"""$typ: $message in $file:$line:$col""")
        }

      }

      val cFile = Paths.get(cFileName).toAbsolutePath
      libraryInterface.addIncludePath("/usr/lib/gcc/x86_64-linux-gnu/7/include/")
      libraryInterface.runClang(cFile.getParent.toString, cFile.getFileName.toString)
    }
  }

  class Instrumenter(cFileName: String) extends TranslationPhase[Core.Specification, Unit] {
    override def translate(spec: Core.Specification) = {
      (sys.props("os.name").toLowerCase(), sys.props("os.arch").toLowerCase()) match {
        case ("linux", "x86_64") => new InstrumenterWorker(spec, cFileName).translate()
        case (os, arch) =>
          Failure(
            Errors.UnsupportedPlatformError(
              os,
              arch,
              "C instrumentation is currently only supported for: linux-x86_64"
            ) :: Nil,
            Nil
          )
      }
    }
  }
}
