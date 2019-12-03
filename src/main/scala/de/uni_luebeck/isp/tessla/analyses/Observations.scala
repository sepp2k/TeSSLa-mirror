package de.uni_luebeck.isp.tessla.analyses

import java.nio.file.{Paths}

import de.uni_luebeck.isp.tessla.{CPatternLexer, CPatternParser, Errors, Location, Tessla, TesslaCore, TranslationPhase}
import de.uni_luebeck.isp.clang_instrumentation.CPPBridge
import de.uni_luebeck.isp.tessla.Errors.{InternalError, ParserError}
import de.uni_luebeck.isp.tessla.CPatternParser.{ArrayAccessContext, DereferenceContext, ParenthesesContext, PatternContext, ReferenceContext, StructUnionAccessArrowContext, StructUnionAccessContext, VariableContext}
import de.uni_luebeck.isp.tessla.TesslaCore.InStreamDescription
import org.antlr.v4.runtime.{BaseErrorListener, CharStreams, CommonTokenStream, RecognitionException, Recognizer, Token}


object Observations {
  sealed abstract class Pattern
  final case class Variable(functionName: Option[String], name: String) extends Pattern {
    override def toString: String = functionName match {
      case Some(value) => s"$value::$name"
      case None => name
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

  class InstrumenterWorker(spec: TesslaCore.Specification, cFileName: String) extends TranslationPhase.Translator[Unit] {
    def parsePattern(str: String, loc: Location): Pattern = {
      val src = CharStreams.fromString(str, loc.path)
      val lexer = new CPatternLexer(src)
      lexer.setLine(loc.range.map(_.fromLine).getOrElse(0))
      lexer.setCharPositionInLine(loc.range.map(_.fromColumn).getOrElse(0))
      val tokens = new CommonTokenStream(lexer)
      val parser = new CPatternParser(tokens)
      parser.removeErrorListeners()
      parser.addErrorListener(new BaseErrorListener {
        override def syntaxError(r: Recognizer[_, _], offendingToken: Any, l: Int, c: Int, msg: String, e: RecognitionException) = {
          error(ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token])))
        }
      })

      val pattern = parser.start().pattern()

      if (parser.getNumberOfSyntaxErrors > 0) {
        abortOnError()
      }

      def translatePattern(context: PatternContext): Pattern = context match {
        case ctx: ArrayAccessContext => ArrayAccess(translatePattern(ctx.pattern()))
        case ctx: ReferenceContext => Reference(translatePattern(ctx.pattern()))
        case ctx: VariableContext => Variable(Option(ctx.functionName).map(_.getText), ctx.name.getText)
        case ctx: StructUnionAccessContext => StructUnionAccess(translatePattern(ctx.pattern()), ctx.ID().getSymbol.getText)
        case ctx: StructUnionAccessArrowContext => StructUnionAccess(translatePattern(ctx.pattern()), ctx.ID().getSymbol.getText, isArrow = true)
        case ctx: DereferenceContext => Dereference(translatePattern(ctx.pattern()))
        case ctx: ParenthesesContext => translatePattern(ctx.pattern())
      }

      translatePattern(pattern)
    }

    private def argumentAsString(annotation: TesslaCore.Annotation, argumentName: String): String = {
      val argument = annotation.arguments.getOrElse(argumentName,
        throw InternalError("Annotation has no argument " + argumentName + ", should have been caught by the (not yet implemented) type checker.", annotation.loc))
      argument match {
        case Tessla.ConstantExpression.Literal(Tessla.StringLiteral(x), _) => x
        case _ => throw InternalError("Expression must be a string, should have been caught by the (not yet implemented) type checker.", argument.loc)
      }
    }

    private def argumentAsInt(annotation: TesslaCore.Annotation, argumentName: String): Int = {
      val argument = annotation.arguments.getOrElse(argumentName,
        throw InternalError("Annotation has no argument " + argumentName + ", should have been caught by the (not yet implemented) type checker.", annotation.loc))
      argument match {
        case Tessla.ConstantExpression.Literal(Tessla.IntLiteral(x), _) => x.toInt
        case _ => throw InternalError("Expression must be an Int, should have been caught by the (not yet implemented) type checker.", argument.loc)
      }
    }

    val threadIdInStreams = spec.inStreams.filter{ in =>
      in.annotations.exists(_.name == "ThreadId")
    }

    protected def encloseInstrumentationCode(code: String): String = {
      val lines = code.split("\n") ++
        threadIdInStreams.map{ in => s"""trace_push_thread_id(events, "${in.name}");"""}
      s"uint8_t* events = trace_create_events(${lines.length});\n" +
        lines.sorted.mkString("\n") +
        "\ntrace_write(events);"
    }

    private def mergeObservations(observations: Seq[(String, String)]*) =
      observations.flatten.groupBy(_._1).map{
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
        createFunctionObservations(annotationName + "Arg",
          (annotation, in) => printEventArgument(in, argumentAsInt(annotation, "index"))))

    private def createFunctionObservations(annotationName: String, createCode: (TesslaCore.Annotation, InStreamDescription) => String): Seq[(String, String)] =
      spec.inStreams.flatMap { in =>
        in.annotations.filter(_.name == annotationName).map { annotation =>
          val name = argumentAsString(annotation, "name")
          val code = createCode(annotation, in)
          (name, code)
        }
      }

    private def assertUnit(annotationName: String): Unit =
      spec.inStreams.foreach { in =>
        in.annotations.filter(_.name == annotationName).foreach { annotation =>
          in.typ.elementType match {
            case TesslaCore.ObjectType(memberTypes) if memberTypes.isEmpty => // good
            case _ => error(Errors.WrongType("Events[Unit]", in.typ, in.loc))
          }
        }}

    private def createFunctionTypeAssertions(annotationName: String) =
      spec.inStreams.flatMap { in =>
        in.annotations.filter(_.name == annotationName).map { annotation =>
          val name = argumentAsString(annotation, "name")
          val argIndex = if (annotation.arguments.contains("index")) {
            argumentAsInt(annotation, "index")
          } else {
            -1
          }
          name -> (argIndex -> in)
        }
      }.groupBy(_._1).mapValues(_.map(_._2))

    private def createPatternObservations(annotationName: String, createCode: (TesslaCore.Annotation, InStreamDescription) => String) =
      spec.inStreams.flatMap { in =>
        in.annotations.filter(_.name == annotationName).map { annotation =>
          val function = if (annotation.arguments.contains("function")) {
            Some(argumentAsString(annotation, "function"))
          } else {
            None
          }
          val lvalue = argumentAsString(annotation, "lvalue")
          val pattern = parsePattern(lvalue, annotation.arguments("lvalue").loc)
          val code = createCode(annotation, in)
          (pattern, function, code)
        }
      }

    protected def printEvent(in: InStreamDescription, value: String): String = in.typ.elementType match {
      case TesslaCore.BuiltInType("Int", Seq()) =>
        s"""trace_push_int(events, "${in.name}", (int64_t) $value);"""
      case TesslaCore.BuiltInType("Float", Seq()) =>
        s"""trace_push_float(events, "${in.name}", (double) $value);"""
      case TesslaCore.BuiltInType("Bool", Seq()) =>
        s"""trace_push_bool(events, "${in.name}", (bool) $value);"""
      case TesslaCore.ObjectType(memberTypes) if memberTypes.isEmpty =>
        s"""trace_push_unit(events, "${in.name}");"""
      case _ => ""
    }

    protected def printEventValue(in: InStreamDescription): String = printEvent(in, "value")

    protected def printEventArgument(in: InStreamDescription, index: Int): String = printEvent(in, s"arg$index")

    protected def printEventIndex(in: InStreamDescription, index: Int): String = printEvent(in, s"index$index")

    protected val prefix = "#include \"logging.h\"\n"

    protected def assertSameType(cType: String, stream: TesslaCore.InStreamDescription): Unit = {
      val types = Map(
        "Int" -> List("int", "byte", "short", "long", "long long", "char", "int64_t", "int32_t", "int16_t", "int8_t", "uint64_t", "uint32_t", "uint16_t", "uint8_t"),
        "Bool" -> List("bool", "_Bool"),
        "Float" -> List("float", "double"),
        "()" -> List("void")
      )
      stream.typ.elementType match {
        case TesslaCore.ObjectType(memberTypes) if memberTypes.isEmpty =>
          if (cType != "void") {
            warn(stream.loc, s"Stream ${stream.name} was declared as ${stream.typ}, but mapped to $cType.")
          }
        case TesslaCore.BuiltInType(name, _) if types.contains(name) =>
          if (!(types(name).contains(cType) || cType.endsWith("*") && name == "Int")) {
            warn(stream.loc, s"Stream ${stream.name} was declared as ${stream.typ}, but mapped to $cType.")
          }
        case _ =>
          val expectedType = if (cType.endsWith("*")) {
            "Events[Int]"
          } else {
            types.collect{
              case (streamType, cTypes) if cTypes.contains(cType) => streamType
            } match {
              case head :: _ => s"Events[$head]"
              case Nil => "Events[Int], Events[Float] or Events[Bool]"
            }
          }
          error(Errors.WrongType(expectedType, stream.typ, stream.loc))
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
      val functionReturnTypeAssertions = merge(createFunctionTypeAssertions("InstFunctionReturn"),
        createFunctionTypeAssertions("InstFunctionReturnArg"))

      val functionReturned = createFunctionObservations("InstFunctionReturned")
      val functionReturnedTypeAssertions = merge(createFunctionTypeAssertions("InstFunctionReturned"),
        createFunctionTypeAssertions("InstFunctionReturnedArg"))

      // TODO merge pattern observations for same pattern into one observation entry and call encloseInstrumentationCode

      val writes = createPatternObservations("InstWrite",
          (annotation, in) => printEventValue(in)) ++
        createPatternObservations("InstWriteIndex",
          (annotation, in) => printEventIndex(in, argumentAsInt(annotation, "index")))

      val writesInFunction = createPatternObservations("InstWriteInFunction",
          (annotation, in) => printEventValue(in)) ++
        createPatternObservations("InstWriteInFunctionIndex",
          (annotation, in) => printEventIndex(in, argumentAsInt(annotation, "index")))

      val reads = createPatternObservations("InstRead",
          (annotation, in) => printEventValue(in)) ++
        createPatternObservations("InstReadIndex",
          (annotation, in) => printEventIndex(in, argumentAsInt(annotation, "index")))

      val readsInFunction = createPatternObservations("InstReadInFunction",
          (annotation, in) => printEventValue(in)) ++
        createPatternObservations("InstReadInFunctionIndex",
          (annotation, in) => printEventIndex(in, argumentAsInt(annotation, "index")))

      val callbacks = functionCall.map { case (name, code) =>
        name + "_call" -> code
      } ++ functionCalled.map { case (name, code) =>
        name + "_called" -> code
      } ++ functionReturn.map { case (name, code) =>
        name + "_return" -> code
      } ++ functionReturned.map { case (name, code) =>
        name + "_returned" -> code
      }

      val libraryInterface = new CPPBridge.LibraryInterface {
        def assertTypes(f: CPPBridge.FunctionDesc, assertions: Map[String, Seq[(Int, InStreamDescription)]]): Unit = {
          assertions.get(f.name).foreach{ seq => seq.foreach {
            case (argIndex, inStream) =>
              if (argIndex >= f.parNum()) {
                error(Errors.InstArgDoesNotExist(f.name, argIndex, inStream.loc))
              } else if (argIndex < 0) {
                assertSameType(f.retType(), inStream)
              } else {
                assertSameType(f.parType(argIndex), inStream)
              }
          }}
        }

        override def checkInstrumentationRequiredFuncReturn(f: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          assertTypes(f, functionReturnTypeAssertions)
          if (functionReturn.contains(f.name)) {
            f.name + "_return"
          } else {
            ""
          }
        }

        override def checkInstrumentationRequiredFuncReturned(f: CPPBridge.FunctionDesc, containingFunc: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          assertTypes(f, functionReturnedTypeAssertions)
          if (functionReturned.contains(f.name)) {
            f.name + "_returned"
          } else {
            ""
          }
        }

        override def checkInstrumentationRequiredFuncCall(f: CPPBridge.FunctionDesc, containingFunc: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          assertTypes(f, functionCallTypeAssertions)
          if (functionCall.contains(f.name)) {
            f.name + "_call"
          } else {
            ""
          }
        }

        override def checkInstrumentationRequiredFuncCalled(f: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          assertTypes(f, functionCalledTypeAssertions)
          if (functionCalled.contains(f.name)) {
            f.name + "_called"
          } else {
            ""
          }
        }

        override def checkInstrumentationRequiredWrite(pattern: String, typ: String, containingFunc: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          // TODO assert types and create callbacks for writes and writesInFunction
          ""
        }

        override def checkInstrumentationRequiredRead(pattern: String, typ: String, containingFunc: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          // TODO assert types and create callbacks for reads and readsInFunction
          ""
        }

        override def getUserCbPrefix: String = prefix

        override def getCallbackCode(cbName : String) : String = {
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

  class Instrumenter(cFileName: String) extends TranslationPhase[TesslaCore.Specification, Unit] {
    override def translate(spec: TesslaCore.Specification) = {
      new InstrumenterWorker(spec, cFileName).translate()
    }
  }
}
