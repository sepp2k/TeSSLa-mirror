package de.uni_luebeck.isp.tessla.analyses

import java.nio.file.{Path, Paths}

import de.uni_luebeck.isp.tessla.{CPatternLexer, CPatternParser, Errors, Location, Tessla, TesslaCore, TranslationPhase}
import de.uni_luebeck.isp.clang_instrumentation.CPPBridge
import de.uni_luebeck.isp.tessla.Errors.{InternalError, ParserError}
import de.uni_luebeck.isp.tessla.CPatternParser.{ArrayContext, DerefContext, MemberContext, PatternContext, RefContext, VariableContext}
import de.uni_luebeck.isp.tessla.TesslaCore.InStreamDescription
import org.antlr.v4.runtime.{BaseErrorListener, CharStreams, CommonTokenStream, RecognitionException, Recognizer, Token}


object Observations {
  case class Pattern(Variable: Option[Variable] = None, ArrayAccess: Option[Pattern] = None, StructUnionAccess: Option[StructUnionAccess] = None, Ref: Option[Pattern] = None, DeRef: Option[Pattern] = None, code: Option[String] = None)
  case class Variable(VarName: String, Function: Option[String] = None)
  case class StructUnionAccess(Base: Pattern, Field: String)

  class InstrumenterWorker(spec: TesslaCore.Specification, cFileName: String) extends TranslationPhase.Translator[Unit] {
    def parsePattern(str: String, loc: Location, function: Option[String] = None): Pattern = {
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
        case ctx: ArrayContext => Pattern(ArrayAccess = Some(translatePattern(ctx.pattern())))
        case ctx: RefContext => Pattern(Ref = Some(translatePattern(ctx.pattern())))
        case ctx: VariableContext => Pattern(Variable = Some(Variable(VarName = ctx.ID().getSymbol.getText, Function = function)))
        case ctx: MemberContext => Pattern(StructUnionAccess = Some(StructUnionAccess(Base = translatePattern(ctx.pattern()), Field = ctx.ID().getSymbol.getText)))
        case ctx: DerefContext => Pattern(DeRef = Some(translatePattern(ctx.pattern())))
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

//    private def merge(functions: Seq[Function]): Seq[Function] =
//      functions.groupBy(_.FunctionName).values.map{ functions =>
//        functions.reduce((a,b) => a.copy(code = a.code + "\n" + b.code))
//      }.toSeq
//
//    private def merge(patterns: Seq[Pattern])(implicit d: DummyImplicit): Seq[Pattern] =
//      patterns.groupBy(_.copy(code = None)).values.map { patterns =>
//        patterns.reduce((a,b) => a.copy(code = a.code.flatMap(aStr => b.code.map(bStr => aStr + "\n" + bStr))))
//      }.toSeq
//
//    private def enclose(functions: Seq[Function]): Seq[Function] =
//      merge(functions).map{ function =>
//        function.copy(code = encloseInstrumentationCode(function.code))
//      }
//
//    private def enclose(patterns: Seq[Pattern])(implicit d: DummyImplicit): Seq[Pattern] =
//      merge(patterns).map { pattern =>
//        pattern.copy(code = pattern.code.map(encloseInstrumentationCode))
//      }
//
    private def createFunctionObservations(annotationName: String, createCode: (TesslaCore.Annotation, InStreamDescription) => String): Map[String, String] =
      spec.inStreams.flatMap { in =>
        in.annotations.filter(_.name == annotationName).map { annotation =>
          val name = argumentAsString(annotation, "name")
          val code = createCode(annotation, in)
          (name, code)
        }
      }.toMap
//
//    private def createPatternObservations(annotationName: String, createCode: (TesslaCore.Annotation, InStreamDescription) => String): Seq[Pattern] =
//      spec.inStreams.flatMap { in =>
//        in.annotations.filter(_.name == annotationName).map { annotation =>
//          val function = if (annotation.arguments.contains("function")) {
//            Some(argumentAsString(annotation, "function"))
//          } else {
//            None
//          }
//          val lvalue = argumentAsString(annotation, "lvalue")
//          val pattern = parsePattern(lvalue, annotation.arguments("lvalue").loc, function)
//          pattern.copy(code = Some(createCode(annotation, in)))
//        }
//      }
//
    protected def printEvent(in: InStreamDescription, value: String): String = in.typ.elementType match {
      case TesslaCore.BuiltInType("Int", Seq()) =>
        s"""trace_push_int(events, "${in.name}", (int64_t) $value);"""
      case TesslaCore.BuiltInType("Float", Seq()) =>
        s"""trace_push_float(events, "${in.name}", (double) $value);"""
      case TesslaCore.BuiltInType("Bool", Seq()) =>
        s"""trace_push_bool(events, "${in.name}", (bool) $value);"""
      case _ =>
        error(Errors.WrongType("Events[Int], Events[Float] or Events[Bool]", in.typ, in.loc))
        ""
    }

    protected def printUnitEvent(in: InStreamDescription): String = in.typ.elementType match {
      case TesslaCore.ObjectType(memberTypes) if memberTypes.isEmpty =>
        s"""trace_push_unit(events, "${in.name}");"""
      case _ =>
        error(Errors.WrongType("Events[Unit]", in.typ, in.loc))
        ""
    }

    protected def printEventValue(in: InStreamDescription): String = printEvent(in, "value")

    protected def printEventIndex(in: InStreamDescription): String = printEvent(in, "index")

    protected def printEventArgument(in: InStreamDescription, index: Int): String = printEvent(in, s"arg$index")

    protected val prefix = "#include \"instrumentation.h\"\n"

    override protected def translateSpec(): Unit = {
//      val functionCalls = createFunctionObservations("InstFunctionCall",
//        (_, in) => printUnitEvent(in))
//
//      val functionCallArgs = createFunctionObservations("InstFunctionCallArg",
//        (annotation, in) => printEventArgument(in, argumentAsInt(annotation, "index")))
//
      val functionCalled = createFunctionObservations("InstFunctionCalled",
        (_, in) => printUnitEvent(in))
//
//      val functionCalledArgs = createFunctionObservations("InstFunctionCalledArg",
//        (annotation, in) => printEventArgument(in, argumentAsInt(annotation, "index")))
//
//      val functionReturns = createFunctionObservations("InstFunctionReturn",
//        (_, in) => printUnitEvent(in)) ++ createFunctionObservations("InstFunctionReturnValue",
//        (_, in) => printEventValue(in))
//
//      val functionReturned = createFunctionObservations("InstFunctionReturned",
//        (_, in) => printUnitEvent(in)) ++ createFunctionObservations("InstFunctionReturnedValue",
//        (_, in) => printEventValue(in))
//
//      val globalAssignments = createPatternObservations("GlobalWrite",
//        (annotation, in) => printEventValue(in)) ++ createPatternObservations("GlobalWriteIndex",
//        (annotation, in) => printEventIndex(in))
//
//      val localAssignments = createPatternObservations("LocalWrite",
//        (annotation, in) => printEventValue(in)) ++ createPatternObservations("LocalWriteIndex",
//        (annotation, in) => printEventIndex(in))
//
//      val globalReads = createPatternObservations("GlobalRead",
//        (annotation, in) => printEventValue(in)) ++ createPatternObservations("GlobalReadIndex",
//        (annotation, in) => printEventIndex(in))
//
//      val localReads = createPatternObservations("LocalRead",
//        (annotation, in) => printEventValue(in)) ++ createPatternObservations("LocalReadIndex",
//        (annotation, in) => printEventIndex(in))
//
//      val observations = Observations(
//        FunctionCalls = enclose(functionCalls ++ functionCallArgs),
//        FunctionCalled = merge(enclose(functionCalled ++ functionCalledArgs) ++ setups),
//        FunctionReturns = merge(enclose(functionReturns) ++ teardowns),
//        FunctionReturned = enclose(functionReturned),
//        Assignments = enclose(globalAssignments ++ localAssignments),
//        VarReads = enclose(globalReads ++ localReads),
//        userCbPrefix = prefix)

      println(cFileName)

      val callbacks = functionCalled.map { case (name, code) =>
        name + "_call" -> code
      }

      val libraryInterface = new CPPBridge.LibraryInterface {
        override def checkInstrumentationRequiredFuncReturn(f: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          ""
        }

        override def checkInstrumentationRequiredFuncReturned(f: CPPBridge.FunctionDesc, containingFunc: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          ""
        }

        override def checkInstrumentationRequiredFuncCall(f: CPPBridge.FunctionDesc, containingFunc: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          ""
        }

        override def checkInstrumentationRequiredFuncCalled(f: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          println("###########")
          println(filename)
          println(line + ", " + col)
          println("-------")
          println(f)

          if (functionCalled.contains(f.name)) {
            f.name + "_call"
          } else {
            ""
          }
        }

        override def checkInstrumentationRequiredWrite(pattern: String, containingFunc: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          println("///////////////////")
          println(pattern)
          println(containingFunc)
          println(filename)
          println(line + ", " + col)
          ""
        }

        override def checkInstrumentationRequiredRead(pattern: String, containingFunc: CPPBridge.FullFunctionDesc, filename: String, line: Int, col: Int) : String = {
          ""
        }

        override def getUserCbPrefix: String = {
          prefix
        }

        override def getCallbackCode(cbName : String) : String = {
          callbacks.getOrElse(cbName, "")
        }
      }

      val cFile = Paths.get(cFileName).toAbsolutePath
      libraryInterface.runClang(cFile.getParent.toString, cFile.getFileName.toString)
    }
  }

  class Instrumenter(cFileName: String) extends TranslationPhase[TesslaCore.Specification, Unit] {
    override def translate(spec: TesslaCore.Specification) = {
      new InstrumenterWorker(spec, cFileName).translate()
    }
  }
}
