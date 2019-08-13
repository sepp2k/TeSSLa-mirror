package de.uni_luebeck.isp.tessla.analyses

import de.uni_luebeck.isp.tessla.{CPatternLexer, CPatternParser, Errors, Location, Tessla, TesslaCore, TranslationPhase}
import Observations._
import de.uni_luebeck.isp.tessla.Errors.{InternalError, ParserError, TesslaErrorWithTimestamp}
import de.uni_luebeck.isp.tessla.CPatternParser.{ArrayContext, DerefContext, MemberContext, PatternContext, RefContext, VariableContext}
import de.uni_luebeck.isp.tessla.TesslaCore.InStreamDescription
import org.antlr.v4.runtime.{BaseErrorListener, CharStreams, CommonTokenStream, RecognitionException, Recognizer, Token}
import spray.json.DefaultJsonProtocol._
import spray.json._

case class Observations(FunctionCalls: Seq[Function] = Seq(),
                        FunctionCalled: Seq[Function] = Seq(),
                        FunctionReturns: Seq[Function] = Seq(),
                        FunctionReturned: Seq[Function] = Seq(),
                        Assignments: Seq[Pattern] = Seq(),
                        VarReads: Seq[Pattern] = Seq(),
                        userCbPrefix: String) {
  override def equals(obj: Any) = obj match {
    case other: Observations =>
      FunctionCalls.sortBy(_.hashCode) == other.FunctionCalls.sortBy(_.hashCode) &&
      FunctionCalled.sortBy(_.hashCode) == other.FunctionCalled.sortBy(_.hashCode) &&
      FunctionReturns.sortBy(_.hashCode) == other.FunctionReturns.sortBy(_.hashCode) &&
      FunctionReturned.sortBy(_.hashCode) == other.FunctionReturned.sortBy(_.hashCode) &&
      Assignments.sortBy(_.hashCode) == other.Assignments.sortBy(_.hashCode) &&
      VarReads.sortBy(_.hashCode) == other.VarReads.sortBy(_.hashCode) &&
      userCbPrefix == other.userCbPrefix
    case _ => false
  }

  override def hashCode() =
    (FunctionCalls.sortBy(_.hashCode), FunctionCalled.sortBy(_.hashCode), FunctionReturns.sortBy(_.hashCode),
      FunctionReturned.sortBy(_.hashCode), Assignments.sortBy(_.hashCode), VarReads.sortBy(_.hashCode), userCbPrefix).hashCode

  override def toString = this.toJson.prettyPrint
}

object Observations {
  case class Function(FunctionName: String, code: String)
  case class Pattern(Variable: Option[Variable] = None, ArrayAccess: Option[Pattern] = None, StructUnionAccess: Option[StructUnionAccess] = None, Ref: Option[Pattern] = None, DeRef: Option[Pattern] = None, code: Option[String] = None)
  case class Variable(VarName: String, Function: Option[String] = None)
  case class StructUnionAccess(Base: Pattern, Field: String)

  implicit val functionFormat = jsonFormat2(Function)
  implicit val variableFormat = jsonFormat2(Variable)
  implicit val structUnionAccessFormat: JsonFormat[StructUnionAccess] = lazyFormat(jsonFormat2(StructUnionAccess))
  implicit val patternFormat: JsonFormat[Pattern] = lazyFormat(jsonFormat6(Pattern))
  implicit val observationsFormat: JsonFormat[Observations] = jsonFormat7(Observations.apply)

  class Generator(pthread: Boolean) extends TranslationPhase[TesslaCore.Specification, Observations] {
    override def translate(spec: TesslaCore.Specification) = new Translator(spec, pthread).translate()
  }

  class Translator(spec: TesslaCore.Specification, pthread: Boolean) extends TranslationPhase.Translator[Observations] {
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

    if (!pthread) {
      threadIdInStreams.foreach { in =>
        error(Errors.BadAnnotation("The annotation @ThreadId cannot be used without enabling pthread support.", in.loc))
      }
    }

    protected def encloseInstrumentationCode(code: String): String =
      (if (pthread) "pthread_mutex_lock(&lock);\n" else "") +
        "uint64_t timestamp = trace_get_normalized_timestamp();\n" +
        code +
        (if (threadIdInStreams.nonEmpty) "pthread_t thread = pthread_self();\n" else "") +
        threadIdInStreams.map{ in => "\n" + printIntEvent(in.name, "thread")}.mkString("") +
        "\n" + "fflush(trace_outfile);" +
        (if (pthread) "\npthread_mutex_unlock(&lock);" else "")

    private def merge(functions: Seq[Function]): Seq[Function] =
      functions.groupBy(_.FunctionName).values.map{ functions =>
        functions.reduce((a,b) => a.copy(code = a.code + "\n" + b.code))
      }.toSeq

    private def merge(patterns: Seq[Pattern])(implicit d: DummyImplicit): Seq[Pattern] =
      patterns.groupBy(_.copy(code = None)).values.map { patterns =>
        patterns.reduce((a,b) => a.copy(code = a.code.flatMap(aStr => b.code.map(bStr => aStr + "\n" + bStr))))
      }.toSeq

    private def enclose(function: Function): Function =
      function.copy(code = encloseInstrumentationCode(function.code))

    private def enclose(pattern: Pattern): Pattern =
      pattern.copy(code = pattern.code.map(encloseInstrumentationCode))

    private def createFunctionObservations(annotationName: String, createCode: (TesslaCore.Annotation, InStreamDescription) => String): Seq[Function] =
      spec.inStreams.flatMap { in =>
        in.annotations.filter(_.name == annotationName).map { annotation =>
          val name = argumentAsString(annotation, "name")
          val code = createCode(annotation, in)
          Function(FunctionName = name, code = code)
        }
      }

    private def createPatternObservations(annotationName: String, createCode: (TesslaCore.Annotation, InStreamDescription) => String): Seq[Pattern] =
      spec.inStreams.flatMap { in =>
        in.annotations.filter(_.name == annotationName).map { annotation =>
          val function = if (annotation.arguments.contains("function")) {
            Some(argumentAsString(annotation, "function"))
          } else {
            None
          }
          val lvalue = argumentAsString(annotation, "lvalue")
          val pattern = parsePattern(lvalue, annotation.arguments("lvalue").loc, function)
          pattern.copy(code = Some(createCode(annotation, in)))
        }
      }

    protected def printUnitEvent(name: String): String =
      s"""fprintf(trace_outfile, "%lu: $name\\n", timestamp);"""

    protected def printIntEvent(name: String, value: String): String =
      s"""fprintf(trace_outfile, "%lu: $name = %ld\\n", timestamp, (int64_t) $value);"""

    protected def printFloatEvent(name: String, value: String): String =
      s"""fprintf(trace_outfile, "%lu: $name = %f\\n", timestamp, (double) $value);"""

    protected def printEvent(in: InStreamDescription, value: String): String = in.typ.elementType match {
      case TesslaCore.BuiltInType("Int", Seq()) => printIntEvent(in.name, value)
      case TesslaCore.BuiltInType("Float", Seq()) => printFloatEvent(in.name, value)
      case typ =>
        error(Errors.WrongType("Events[Int] or Events[Float]", in.typ, in.loc))
        ""
    }

    protected def printUnitEvent(in: InStreamDescription): String = in.typ.elementType match {
      case TesslaCore.ObjectType(memberTypes) if memberTypes.isEmpty => printUnitEvent(in.name)
      case typ =>
        error(Errors.WrongType("Events[Unit]", in.typ, in.loc))
        ""
    }

    protected def printEventValue(in: InStreamDescription): String = printEvent(in, "value")

    protected def printEventArgument(in: InStreamDescription, index: Int): String = printEvent(in, s"arg$index")

    protected val setup = Function("main", code = """trace_setup();""")
    protected val prefix = if (pthread) "#include \"instrumentation-pthread.h\"\n" else "#include \"instrumentation.h\"\n"

    override protected def translateSpec() = {
      val functionCalls = createFunctionObservations("InstFunctionCall",
        (_, in) => printUnitEvent(in))

      val functionCallArgs = createFunctionObservations("InstFunctionCallArg",
        (annotation, in) => printEventArgument(in, argumentAsInt(annotation, "index")))

      val functionCalled = createFunctionObservations("InstFunctionCalled",
        (_, in) => printUnitEvent(in))

      val functionCalledArgs = createFunctionObservations("InstFunctionCalledArg",
        (annotation, in) => printEventArgument(in, argumentAsInt(annotation, "index")))

      val functionReturns = createFunctionObservations("InstFunctionReturn",
        (_, in) => printEventValue(in))

      val functionReturned = createFunctionObservations("InstFunctionReturned",
        (_, in) => printEventValue(in))

      val globalAssignments = createPatternObservations("GlobalWrite",
        (annotation, in) => printEventValue(in))

      val localAssignments = createPatternObservations("LocalWrite",
        (annotation, in) => printEventValue(in))

      val globalReads = createPatternObservations("GlobalRead",
        (annotation, in) => printEventValue(in))

      val localReads = createPatternObservations("LocalRead",
        (annotation, in) => printEventValue(in))

      val observations = Observations(
        FunctionCalls = merge(functionCalls ++ functionCallArgs).map(enclose),
        FunctionCalled = merge(merge(functionCalled ++ functionCalledArgs).map(enclose) :+ setup),
        FunctionReturns = merge(functionReturns).map(enclose),
        FunctionReturned = merge(functionReturned).map(enclose),
        Assignments = merge(globalAssignments ++ localAssignments).map(enclose),
        VarReads = merge(globalReads ++ localReads).map(enclose),
        userCbPrefix = prefix)

      observations
    }
  }
}
