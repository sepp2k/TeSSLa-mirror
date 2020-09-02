package de.uni_luebeck.isp.tessla.core.analyses

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
import org.antlr.v4.runtime.{BaseErrorListener, CharStreams, CommonTokenStream, RecognitionException, Recognizer, Token}
import de.uni_luebeck.isp.tessla.core.util._

import scala.collection.mutable
import scala.reflect.ClassTag
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

  class InstrumenterWorker(spec: Core.Specification, cFileName: String, inclPath: Seq[String])
      extends TranslationPhase.Translator[Unit] {

    type Annotation = (String, Core.ExpressionArg)
    type InStream = (Core.Identifier, Core.Type)

    val supportedPlatforms = Set(
      ("linux", "amd64")
    )

    val threadIdInStreams = spec.in.filter {
      case (_, (_, annotations)) => annotations.keySet.contains("ThreadId")
    }

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

    private def argumentAsString(annotation: Annotation, argumentName: String): String =
      argumentAs[String](annotation, argumentName) {
        case s: Core.StringLiteralExpression => s.value
      }

    private def argumentAsInt(annotation: Annotation, argumentName: String): Int =
      argumentAs[Int](annotation, argumentName) {
        case n: Core.IntLiteralExpression => n.value.intValue
      }

    private def argumentAs[T: ClassTag](annotation: Annotation, argumentName: String)(
      convert: PartialFunction[Core.ExpressionArg, T]
    ): T = {
      val loc = annotation._2.location
      val argument = annotationArgs(annotation).getOrElse(
        argumentName,
        throw Errors.UndefinedNamedArg(argumentName, loc)
      )

      convert.applyOrElse(
        argument,
        { arg: Core.ExpressionArg =>
          val tpe = implicitly[ClassTag[T]].runtimeClass.getSimpleName
          throw InternalError(s"Failed to convert argument $argumentName to $tpe.", arg.location)
        }
      )
    }

    private def annotationArgs(annotation: Annotation): Map[String, Core.ExpressionArg] =
      annotation._2 match {
        case r: Core.RecordConstructorExpression => r.entries.mapVals(_._1)
        case _                                   => Map()
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

    def createFunctionCbName(cbSuffix: String)(annotation: Annotation): String =
      argumentAsString(annotation, "name") + cbSuffix

    def createPatternCbName(cbSuffix: String)(annotation: Annotation): String = {
      val function = Try(argumentAsString(annotation, "function")).toOption
      val lvalue = argumentAsString(annotation, "lvalue")
      def scoped(p: Pattern): Pattern = p match {
        case v: Variable if function.isDefined => v.copy(functionName = function)
        case v: Variable                       => v
        case s: StructUnionAccess              => s.copy(base = scoped(s.base))
        case s: ArrayAccess                    => s.copy(base = scoped(s.base))
        case s: Reference                      => s.copy(base = scoped(s.base))
        case s: Dereference                    => s.copy(base = scoped(s.base))
      }

      val pattern = scoped(parsePattern(lvalue, annotation._2.location))
      patternCb(pattern) + cbSuffix
    }

    private def createFunctionObservations(annotationName: String, cbSuffix: String): Map[String, String] = {
      val createObs = createObservations(createFunctionCbName(cbSuffix)) _

      mergeObservations(
        createObs(annotationName, (_, in) => printEventValue(in)),
        createObs(
          annotationName + "Arg",
          (annotation, in) => printEventArgument(in, argumentAsInt(annotation, "index"))
        )
      )
    }

    private def createPatternObservations(annotationName: String, cbSuffix: String): Map[String, String] = {
      val createObs = createObservations(createPatternCbName(cbSuffix)) _

      mergeObservations(
        createObs(annotationName, (_, in) => printEventValue(in)),
        createObs(
          annotationName + "Index",
          (annotation, in) => printEventIndex(in, argumentAsInt(annotation, "index"))
        )
      )
    }

    def createObservations(
      createCbName: Annotation => String
    )(annotationName: String, createCode: (Annotation, InStream) => String): Seq[(String, String)] = {
      annotationsByName(annotationName).map {
        case (annotation, inStream) =>
          val name = createCbName(annotation)
          val code = createCode(annotation, inStream)
          (name, code)
      }
    }

    private def unwrapType(streamType: Core.Type) = streamType match {
      case Core.InstantiatedType("Events", t :: Nil, _) => t
      case _                                            => throw Errors.InternalError(s"Failed to unwrap type $streamType.")
    }

    private def assertUnit(annotationName: String): Unit =
      annotationsByName(annotationName).foreach {
        case (_, (id, typ)) =>
          unwrapType(typ) match {
            case r: Core.RecordType if r.entries.isEmpty => //good
            case _                                       => error(Errors.WrongType("Events[Unit]", typ, id.location))
          }
      }

    private def createPatternTypeAssertions(annotationName: String, cbSuffix: String): Map[String, InStream] =
      annotationsByName(annotationName).map {
        case (annotation, inStream) =>
          createPatternCbName(cbSuffix)(annotation) -> inStream
      }.toMap

    private def createFunctionTypeAssertions(
      annotationName: String,
      cbSuffix: String
    ): Map[String, Seq[(Int, InStream)]] =
      annotationsByName(annotationName)
        .map {
          case (annotation, inStream) =>
            val name = createFunctionCbName(cbSuffix)(annotation)
            val argIndex = Try(argumentAsInt(annotation, "index")) getOrElse -1
            name -> (argIndex -> inStream)
        }
        .groupMap(_._1)(_._2)

    private var c = 0
    private val memoize = mutable.HashMap[Pattern, String]()
    def patternCb(pattern: Pattern) = {
      memoize.getOrElseUpdate(
        pattern, {
          val patStr = pattern.toString
          if (patStr.matches("""^[a-zA-Z_][\w_]*$""")) patStr
          else {
            c += 1
            s"_$c"
          }
        }
      )
    }

    def annotationsByName(annotationName: String): Seq[(Annotation, InStream)] = {
      spec.in.toSeq.flatMap {
        case (id, (streamType, annotations)) =>
          annotations
            .filter(_._1 == annotationName)
            .flatMap { case (name, args) => args.map(name -> _) }
            .map((_, (id, streamType)))
      }
    }

    protected def printEvent(in: InStream, value: String): String = {
      val name = in._1.fullName

      unwrapType(in._2) match {
        case t: Core.InstantiatedType if t.name == Core.IntType.name =>
          s"""trace_push_int(events, "$name", (int64_t) $value);"""
        case t: Core.InstantiatedType if t.name == Core.FloatType.name =>
          s"""trace_push_float(events, "$name", (double) $value);"""
        case t: Core.InstantiatedType if t.name == Core.BoolType.name =>
          s"""trace_push_bool(events, "$name", (bool) $value);"""
        case r: Core.RecordType if r.entries == Core.UnitType.entries =>
          s"""trace_push_unit(events, "$name");"""
        case t => s"$t"
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
        case r: Core.RecordType if r.entries.isEmpty =>
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
      val (os, arch) = (sys.props("os.name").toLowerCase(), sys.props("os.arch").toLowerCase())
      if (!supportedPlatforms.contains((os, arch)))
        throw Errors.InstrUnsupportedPlatform(os, arch, supportedPlatforms)

      assertUnit("InstFunctionCall")
      assertUnit("InstFunctionCalled")

      val functionAnnotations = Map(
        "InstFunctionCall" -> "_call",
        "InstFunctionCalled" -> "_called",
        "InstFunctionReturn" -> "_return",
        "InstFunctionReturned" -> "_returned"
      )
      val functionTypeAssertions = functionAnnotations
        .map {
          case (name, suffix) =>
            merge(
              createFunctionTypeAssertions(name, suffix),
              createFunctionTypeAssertions(s"${name}Arg", suffix)
            )
        }
        .reduce(_ ++ _)
      val functionObservations = functionAnnotations.map((createFunctionObservations _).tupled)

      val patternAnnotations = Map(
        "GlobalWrite" -> "_globalWrite",
        "GlobalRead" -> "_globalRead",
        "LocalWrite" -> "_localWrite",
        "LocalRead" -> "_localRead"
      )
      val patternObservations = patternAnnotations.map((createPatternObservations _).tupled)
      // TODO: What about GlobalWriteIndex etc.?
      val patternTypeAssertions = patternAnnotations.map((createPatternTypeAssertions _).tupled).reduce(_ ++ _)

      val callbacks = (functionObservations ++ patternObservations).reduce(_ ++ _)

      val libraryInterface: CPPBridge.LibraryInterface = new CPPBridge.LibraryInterface {
        def assertFuncTypes(f: CPPBridge.FunctionDesc, assertions: Map[String, Seq[(Int, InStream)]]): Unit = {
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

        def assertPatternTypes(cb: String, typ: String, assertions: Map[String, InStream]): Unit =
          assertions.get(cb).foreach(assertSameType(typ, _))

        def checkFunc(f: CPPBridge.FunctionDesc, suffix: String) = {
          assertFuncTypes(f, functionTypeAssertions)
          val cbName = f.name + suffix
          Option.when(callbacks.contains(cbName))(cbName) getOrElse ""
        }

        override def checkInstrumentationRequiredFuncReturn(
          f: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String = checkFunc(f, "_return")

        override def checkInstrumentationRequiredFuncReturned(
          f: CPPBridge.FunctionDesc,
          containingFunc: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String = checkFunc(f, "_returned")

        override def checkInstrumentationRequiredFuncCall(
          f: CPPBridge.FunctionDesc,
          containingFunc: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String = checkFunc(f, "_call")

        override def checkInstrumentationRequiredFuncCalled(
          f: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String = checkFunc(f, "_called")

        def checkPattern(pattern: String, suffix: String, typ: String) = {
          val pat = parsePattern(pattern, Location.unknown)
          val cbName = patternCb(pat) + suffix
          assertPatternTypes(cbName, typ, patternTypeAssertions)
          Option.when(callbacks.contains(cbName))(cbName)
        }

        override def checkInstrumentationRequiredWrite(
          pattern: String,
          typ: String,
          containingFunc: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String =
          checkPattern(pattern, "_globalWrite", typ) orElse
            checkPattern(pattern, "_localWrite", typ) getOrElse ""

        override def checkInstrumentationRequiredRead(
          pattern: String,
          typ: String,
          containingFunc: CPPBridge.FullFunctionDesc,
          filename: String,
          line: Int,
          col: Int
        ): String =
          checkPattern(pattern, "_globalRead", typ) orElse
            checkPattern(pattern, "_localRead", typ) getOrElse ""

        override def getUserCbPrefix: String = prefix

        override def getCallbackCode(cbName: String): String = {
          callbacks.getOrElse(cbName, "")
        }

        override def reportDiagnostic(typ: String, message: String, file: String, line: Int, col: Int): Unit = {
          System.err.println(s"""$typ: $message in $file:$line:$col""")
        }

      }

      val cFile = Paths.get(cFileName).toAbsolutePath
      inclPath.foreach(libraryInterface.addIncludePath)
      libraryInterface.runClang(cFile.getParent.toString, cFile.getFileName.toString)
    }
  }

  class Instrumenter(cFileName: String, inclPath: Seq[String] = Seq())
      extends TranslationPhase[Core.Specification, Unit] {

    override def translate(spec: Core.Specification): TranslationPhase.Result[Unit] =
      new InstrumenterWorker(spec, cFileName, inclPath).translate()

  }
}
