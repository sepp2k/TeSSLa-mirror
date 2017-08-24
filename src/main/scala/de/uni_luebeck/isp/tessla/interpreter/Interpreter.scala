package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{AstToCore, CompilationError, Compiler, Location, TesslaCore, TesslaSource, TimeUnit, TranslationPhase, Types, UnknownLoc}
import de.uni_luebeck.isp.tessla.TranslationPhase.Result
import shapeless.{::, HNil}

import scala.io.Source

class Interpreter(val spec: TesslaCore.Specification) extends Specification[BigInt] {
  val inStreams: Map[String, (Input[TesslaCore.Value], Types.ValueType)] = spec.inStreams.map {
    case (name, typ, _) =>
      name -> (Input[TesslaCore.Value](), typ.elementType)
  }.toMap

  lazy val defs: Map[String, Lazy[Stream[TesslaCore.Value]]] = inStreams.mapValues {
    case (inputStream, _) => Lazy(inputStream)
  } ++ spec.streams.map {
    case (name, exp) => (name, Lazy(eval(exp)))
  }

  lazy val outStreams: Map[String, Stream[TesslaCore.Value]] = spec.outStreams.map {
    case (name, streamRef) => name -> defs(streamRef.name).get
  }.toMap

  private def evalStream(arg: TesslaCore.StreamRef): Stream[TesslaCore.Value] = arg match {
    case TesslaCore.Stream(name, loc) =>
      defs.getOrElse(name, throw AstToCore.InternalError(s"Couldn't find stream named $name", loc)).get
    case TesslaCore.InputStream(name, loc) =>
      inStreams.getOrElse(name, throw AstToCore.InternalError(s"Couldn't find stream named $name", loc))._1
    case TesslaCore.Nil(_) => nil
  }

  private def eval(exp: TesslaCore.Expression): Stream[TesslaCore.Value] = exp match {
    case TesslaCore.Lift(op, Seq(), loc) =>
      try {
        op.eval(Seq(), loc) match {
          case None => nil
          case Some(x) =>
            nil.default(x)
        }
      } catch {
        case c: CompilationError =>
          nil.default(TesslaCore.ErrorValue(c))
      }
    case TesslaCore.Lift(op, argStreams, loc) =>
      lift(argStreams.map(evalStream)) { args =>
        try {
          op.eval((args, argStreams).zipped.map((arg, s) => arg.withLoc(s.loc)), loc)
        } catch {
          case c: CompilationError =>
            Some(TesslaCore.ErrorValue(c))
        }
      }
    case TesslaCore.Default(values, defaultValue, _) =>
      evalStream(values).default(defaultValue)
    case TesslaCore.DefaultFrom(values, defaults, _) =>
      evalStream(values).default(evalStream(defaults))
    case TesslaCore.Last(values, clock, _) =>
      last(evalStream(clock), evalStream(values))
    case TesslaCore.DelayedLast(values, delays, loc) =>
      delayedLast(intStream(evalStream(delays), loc), evalStream(values))
    case TesslaCore.Time(values, _) =>
      intStreamToValueStream(evalStream(values).time())
  }

  def intStream(stream: Stream[TesslaCore.Value], loc: Location): Stream[BigInt] = {
    lift(stream :: HNil) {
      (args: TesslaCore.Value :: HNil) =>
        args match {
          case TesslaCore.IntLiteral(i, _) :: HNil => Some(i)
          case value :: HNil => throw Types.TypeMismatch(Types.Int, value.typ, loc)
        }
    }
  }

  def intStreamToValueStream(stream: Stream[BigInt]): Stream[TesslaCore.Value] = {
    lift(stream :: HNil) {
      (args: BigInt :: HNil) =>
        args match {
          case i :: HNil => Some(TesslaCore.IntLiteral(i, UnknownLoc))
        }
    }
  }
}

object Interpreter {
  class CoreToInterpreterSpec extends TranslationPhase[TesslaCore.Specification, Interpreter] {
    def translateSpec(spec: TesslaCore.Specification): Interpreter = new Interpreter(spec)
  }

  def fromSource(source: Source, timeUnit: Option[TimeUnit.TimeUnit]): Result[Interpreter] = {
    new Compiler().applyPasses(new TesslaSource(source), timeUnit).andThen(new CoreToInterpreterSpec)
  }

  def fromString(tesslaSource: String, timeUnit: Option[TimeUnit.TimeUnit]): Result[Interpreter] = {
    fromSource(Source.fromString(tesslaSource), timeUnit)
  }

  def fromFile(file: String, timeUnit: Option[TimeUnit.TimeUnit]): Result[Interpreter] = {
    fromSource(Source.fromFile(file), timeUnit)
  }

}