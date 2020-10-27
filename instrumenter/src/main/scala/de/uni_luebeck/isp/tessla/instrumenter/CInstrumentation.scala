package de.uni_luebeck.isp.tessla.instrumenter

import de.uni_luebeck.isp.tessla.core.util._
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
import de.uni_luebeck.isp.tessla.core.Errors.{InternalError, ParserError}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.{CPatternLexer, CPatternParser, Errors, Location, TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.instrumenter.CInstrumentationBridge.{isPlatformSupported, supportedPlatforms}
import org.antlr.v4.runtime.{BaseErrorListener, CharStreams, CommonTokenStream, RecognitionException, Recognizer, Token}

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try

/**
 * Contains data structures and a functionality used to instrument C code depending on the provided annotated
 * TeSSLa specification. This makes use of the annotations defined in the standard library InstrumentationAnnotations module,
 * which denote which parts of the code should be instrumented and which events should be produced at those locations.
 */
object CInstrumentation {

  /**
   * Describes a C function
   */
  trait IFunDesc {

    /**
     * The name of the function
     */
    def name: String

    /**
     * The return type of the function
     */
    def retType: String

    /**
     * The amount of parameters the function declares
     */
    def parNum: Int

    /**
     * The type of the i-th parameter
     * @param i the index of the parameter
     * @return the type of the parameter
     */
    def parType(i: Int): String

    override def toString: String = {
      val args = (1 to parNum).map(i => parType(i - 1)).mkString(", ")
      s"$name: ($args) => $retType"
    }
  }

  /**
   * Extension of a function description by parameter names
   */
  trait IFullFunDesc extends IFunDesc {

    /**
     * The name of the i-th parameter of this function
     * @param i the index of the parameter
     * @return the name of the parameter
     */
    def parName(i: Int): String

    override def toString: String = {
      val args = (1 to parNum).map(i => s"${parName(i - 1)}: ${parType(i - 1)}").mkString(", ")
      s"$name: ($args) => $retType"
    }
  }

  /**
   * Defines the callbacks an instrumentation library has to define. The `check` callbacks are called whenever their
   * respective action (e.g. call, return) occurs in the source code, returning the name of the callback function to
   * be generated in C code, or an empty String if no callback should be generated for this action.
   */
  trait ILibraryInterface {

    /**
     * Called when the function returns.
     * @return the name of the callback to generate, or an empty String.
     */
    def checkInstFuncReturn(f: IFullFunDesc, file: String, line: Int, col: Int): String

    /**
     * Called when the function returned.
     * @return the name of the callback to generate, or an empty String.
     */
    def checkInstFuncReturned(f: IFunDesc, parent: IFullFunDesc, file: String, line: Int, col: Int): String

    /**
     * Called when the function gets called.
     * @return the name of the callback to generate, or an empty String.
     */
    def checkInstFuncCall(f: IFunDesc, parent: IFullFunDesc, file: String, line: Int, col: Int): String

    /**
     * Called when the function got called.
     * @return the name of the callback to generate, or an empty String.
     */
    def checkInstFuncCalled(f: IFullFunDesc, file: String, line: Int, col: Int): String

    /**
     * Called when a write operation with this pattern occurs.
     * @return the name of the callback to generate, or an empty String.
     */
    def checkInstWrite(pattern: String, `type`: String, parent: IFullFunDesc, file: String, line: Int, col: Int): String

    /**
     * Called when a read operation with this pattern occurs.
     * @return the name of the callback to generate, or an empty String.
     */
    def checkInstRead(pattern: String, `type`: String, parent: IFullFunDesc, file: String, line: Int, col: Int): String

    /**
     * Used to define a code prefix, e.g. for includes.
     */
    def getUserCbPrefix: String

    /**
     * Return the code to use for the provided callback function name.
     * @param cbName the name of the callback function
     * @return the body of the callback function in C code.
     */
    def getCallbackCode(cbName: String): String

    /**
     * Reports warnings or errors which occurred during the instrumentation.
     */
    def reportDiagnostic(`type`: String, message: String, file: String, line: Int, col: Int): Unit
  }

  /**
   * Describes an arbitrary C pattern.
   */
  sealed abstract class Pattern

  /**
   * Describes a variable, optionally prepended by a function name. E.g. foo::x or y.
   */
  final case class Variable(functionName: Option[String], name: String) extends Pattern {
    override def toString: String = functionName match {
      case Some(value) => s"$value::$name"
      case None        => name
    }
  }

  /**
   * Describes access of a field on a C struct, e.g. foo.a or foo->a.
   */
  final case class StructUnionAccess(base: Pattern, fieldName: String, isArrow: Boolean = false) extends Pattern {
    override def toString: String = if (isArrow) {
      s"$base->$fieldName"
    } else {
      s"$base.$fieldName"
    }
  }

  /**
   * Describes accessing an array, e.g. foo[].
   * @param base
   */
  final case class ArrayAccess(base: Pattern) extends Pattern {
    override def toString: String = s"$base[]"
  }

  /**
   * Describes a reference `&`
   */
  final case class Reference(base: Pattern) extends Pattern {
    override def toString: String = s"&($base)"
  }

  /**
   * Describes a de-reference `*`
   */
  final case class Dereference(base: Pattern) extends Pattern {
    override def toString: String = s"*($base)"
  }

  /**
   * Translation phase which creates a library interface from a given specification.
   * This means that the interface already takes into consideration which annotations are used in the specification,
   * to only return callback function names where callbacks should be generated, and type-checking the generated events
   * versus the declared event stream in TeSSLa.
   */
  object LibraryInterfaceFactory extends TranslationPhase[Core.Specification, ILibraryInterface] {
    override def translate(spec: Core.Specification): TranslationPhase.Result[ILibraryInterface] =
      new LibraryInterfaceFactoryWorker(spec).translate()
  }

  /**
   * The translator class associated with [[LibraryInterfaceFactory]].
   * @param spec the specification to use
   */
  class LibraryInterfaceFactoryWorker(spec: Core.Specification) extends TranslationPhase.Translator[ILibraryInterface] {

    type Annotation = (String, Core.ExpressionArg)
    type InStream = (Core.Identifier, Core.Type)

    /**
     * A collection of input streams which use the `ThreadId` annotation, since those will have events generated in
     * every instrumentation callback.
     */
    val threadIdInStreams = spec.in.filter {
      case (_, (_, annotations)) => annotations.keySet.contains("ThreadId")
    }

    /**
     * Parses the given String to its corresponding C pattern.
     * @param str the String to translate
     * @param loc the location
     * @return the resulting C pattern
     */
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

    private def encloseInstrumentationCode(code: String): String = {
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

    private def createFunctionCbName(cbSuffix: String)(annotation: Annotation): String =
      argumentAsString(annotation, "name") + cbSuffix

    private def createPatternCbName(cbSuffix: String)(annotation: Annotation): String = {
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

    private def createObservations(
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

    private def annotationsByName(annotationName: String): Seq[(Annotation, InStream)] = {
      spec.in.toSeq.flatMap {
        case (id, (streamType, annotations)) =>
          annotations
            .filter(_._1 == annotationName)
            .flatMap { case (name, args) => args.map(name -> _) }
            .map((_, (id, streamType)))
      }
    }

    /**
     * Produces code to generate an event for the provided input stream, with the given value.
     * @param in the input stream to push the event to
     * @param value the value of the event
     * @return the resulting C code
     */
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

    /**
     * Produces code to print the event value to the provided input stream.
     */
    protected def printEventValue(in: InStream): String = printEvent(in, "value")

    /**
     * Produces code to print the argument name of the specified index to the provided input stream.
     */
    protected def printEventArgument(in: InStream, index: Int): String = printEvent(in, s"arg$index")

    /**
     * Produces code to print the event index to the provided input stream.
     */
    protected def printEventIndex(in: InStream, index: Int): String = printEvent(in, s"index$index")

    protected val prefix = "#include \"logging.h\"\n"

    val alreadyTypeAssertedStreams = mutable.Set.empty[InStream]

    private def assertSameType(cType: String, stream: InStream): Unit = {
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

    override protected def translateSpec(): ILibraryInterface = {
      if (!isPlatformSupported)
        throw Errors.InstrUnsupportedPlatform(supportedPlatforms)

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

      new ILibraryInterface {
        def assertFuncTypes(f: IFunDesc, assertions: Map[String, Seq[(Int, InStream)]]): Unit = {
          assertions.get(f.name).foreach { seq =>
            seq.foreach {
              case (argIndex, inStream) =>
                if (argIndex >= f.parNum) {
                  error(Errors.InstArgDoesNotExist(f.name, argIndex, inStream._1.location))
                } else if (argIndex < 0) {
                  assertSameType(f.retType, inStream)
                } else {
                  assertSameType(f.parType(argIndex), inStream)
                }
            }
          }
        }

        def assertPatternTypes(cb: String, typ: String, assertions: Map[String, InStream]): Unit =
          assertions.get(cb).foreach(assertSameType(typ, _))

        def checkFunc(f: IFunDesc, suffix: String) = {
          assertFuncTypes(f, functionTypeAssertions)
          val cbName = f.name + suffix
          Option.when(callbacks.contains(cbName))(cbName) getOrElse ""
          // println(s"${suffix.drop(1).toUpperCase} $f")
        }

        override def checkInstFuncReturn(f: IFullFunDesc, file: String, line: Int, col: Int): String =
          checkFunc(f, "_return")

        override def checkInstFuncReturned(
          f: IFunDesc,
          parent: IFullFunDesc,
          file: String,
          line: Int,
          col: Int
        ): String =
          checkFunc(f, "_returned")

        override def checkInstFuncCall(f: IFunDesc, parent: IFullFunDesc, file: String, line: Int, col: Int): String =
          checkFunc(f, "_call")

        override def checkInstFuncCalled(f: IFullFunDesc, file: String, line: Int, col: Int): String =
          checkFunc(f, "_called")

        def checkPattern(pattern: String, suffix: String, typ: String): Option[String] = {
          val pat = parsePattern(pattern, Location.unknown)
          val cbName = patternCb(pat) + suffix
          assertPatternTypes(cbName, typ, patternTypeAssertions)
          Option.when(callbacks.contains(cbName))(cbName)
          // println(s"${suffix.drop(1).toUpperCase} $typ $pattern")
        }

        override def checkInstWrite(
          pattern: String,
          typ: String,
          parent: IFullFunDesc,
          file: String,
          line: Int,
          col: Int
        ): String =
          checkPattern(pattern, "_globalWrite", typ) orElse
            checkPattern(pattern, "_localWrite", typ) getOrElse ""

        override def checkInstRead(
          pattern: String,
          typ: String,
          parent: IFullFunDesc,
          file: String,
          line: Int,
          col: Int
        ): String =
          checkPattern(pattern, "_globalRead", typ) orElse
            checkPattern(pattern, "_localRead", typ) getOrElse ""

        override def getUserCbPrefix: String = prefix

        override def getCallbackCode(cbName: String): String = callbacks.getOrElse(cbName, "")

        override def reportDiagnostic(typ: String, message: String, file: String, line: Int, col: Int): Unit = {
          System.err.println(s"""$typ: $message in $file:$line:$col""")
        }
      }
    }
  }

}
