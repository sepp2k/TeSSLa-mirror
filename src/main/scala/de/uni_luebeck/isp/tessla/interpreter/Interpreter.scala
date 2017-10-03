package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.{InternalError, TesslaError, TypeMismatch}
import de.uni_luebeck.isp.tessla.{AstToCore, Compiler, Location, TesslaCore, TesslaSource, TimeUnit, TranslationPhase, Types, UnknownLoc}
import de.uni_luebeck.isp.tessla.TranslationPhase.Result
import shapeless.{::, HNil}

import scala.io.Source

class Interpreter(val spec: TesslaCore.Specification) extends Specification {
  val inStreams: Map[String, (Input, Types.ValueType)] = spec.inStreams.map {
    case (name, typ, _) =>
      name -> (Input, typ.elementType)
  }.toMap


  lazy val defs: Map[String, Lazy[Stream]] = inStreams.mapValues {
    case (inputStream, _) => Lazy(inputStream)
  } ++ spec.streams.map {
    case (name, exp) => (name, Lazy(eval(exp)))
  }

  lazy val outStreams: Map[String, Stream] = spec.outStreams.map {
    case (name, streamRef) => name -> defs(streamRef.name).get
  }.toMap

  private def evalStream(arg: TesslaCore.StreamRef): Stream = arg match {
    case TesslaCore.Stream(name, loc) =>
      defs.getOrElse(name, throw InternalError(s"Couldn't find stream named $name", loc)).get
    case TesslaCore.InputStream(name, loc) =>
      inStreams.getOrElse(name, throw InternalError(s"Couldn't find stream named $name", loc))._1
    case TesslaCore.Nil(_) => nil
  }

  private def eval(exp: TesslaCore.Expression): Stream = exp match {
    case TesslaCore.Lift(op, Seq(), loc) =>
      try {
        op.eval(Seq(), loc) match {
          case None => nil
          case Some(x) =>
            nil.default(x)
        }
      } catch {
        case c: TesslaError =>
          nil.default(TesslaCore.ErrorValue(c))
      }
    case TesslaCore.Lift(op, argStreams, loc) =>
      lift(argStreams.map(evalStream)) { args =>
        try {
          op.eval((args, argStreams).zipped.map((arg, s) => arg.withLoc(s.loc)), loc)
        } catch {
          case c: TesslaError =>
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
      evalStream(values).time()
  }

  def intStream(stream: Stream, loc: Location): Stream = {
    lift(stream :: HNil) {
      (args: TesslaCore.Value :: HNil) =>
        args match {
          case TesslaCore.IntLiteral(i, _) :: HNil => Some(TesslaCore.IntLiteral(i, loc))
          case value :: HNil => throw TypeMismatch(Types.Int, value.typ, loc)
        }
    }
  }


  def addOutStreamListener(callback: (BigInt, String, TesslaCore.Value) => Unit) : Unit = {
    outStreams.foreach {
      case (name, stream) => stream.addListener{_.foreach(callback(getTime, name, _))}
    }
  }
}

object Interpreter {
  class CoreToInterpreterSpec extends TranslationPhase[TesslaCore.Specification, Interpreter] {
    def translateSpec(spec: TesslaCore.Specification): Interpreter = new Interpreter(spec)
  }

  def fromTesslaSource(tesslaSource: TesslaSource, timeUnit: Option[TimeUnit.TimeUnit]) = {
    new Compiler().applyPasses(tesslaSource, timeUnit).andThen(new CoreToInterpreterSpec)
  }

  def fromSource(source: Source, timeUnit: Option[TimeUnit.TimeUnit]): Result[Interpreter] = {
    fromTesslaSource(new TesslaSource(source), timeUnit)
  }

  def fromString(tesslaSource: String, timeUnit: Option[TimeUnit.TimeUnit]): Result[Interpreter] = {
    fromTesslaSource(TesslaSource.fromString(tesslaSource), timeUnit)
  }

  def fromFile(file: String, timeUnit: Option[TimeUnit.TimeUnit]): Result[Interpreter] = {
    fromTesslaSource(TesslaSource.fromFile(file), timeUnit)
  }

}