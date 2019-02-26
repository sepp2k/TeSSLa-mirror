package de.uni_luebeck.isp.tessla

import TranslationPhase._
import de.uni_luebeck.isp.tessla.Errors._

import scala.collection.mutable.ArrayBuffer

/**
  * Trait representing a phase of the translation process.
  *
  * Note that this trait contains mutable members and should only be extended by classes, not objects.
  *
  * @tparam T The type of representation that this phase will be applied to
  * @tparam U The result of applying this phase
  */
trait TranslationPhase[-T, +U] {
  protected def translateSpec(spec: T): U

  protected val warnings = ArrayBuffer[Diagnostic]()
  protected val errors = ArrayBuffer[TesslaError]()

  protected def warn(diagnostic: Diagnostic): Unit = {
    warnings += diagnostic
  }

  protected def error(error: TesslaError) {
    errors += error
  }

  protected def warn(loc: Location, message: String): Unit = warn(SimpleWarning(loc, message))

  protected def tryWithDefault[R](default: => R)(body: => R): R = {
    try {
      body
    } catch {
      case ex: TesslaError =>
        errors += ex
        default
    }
  }

  protected def abortOnError(): Unit = {
    if (errors.nonEmpty) {
      val lastError = errors.remove(errors.length - 1)
      throw lastError
    }
  }

  protected def abort(): Nothing = {
    abortOnError()
    throw InternalError("abort() was called when no errors were present")
  }

  def translate(spec: T): Result[U] = {
    try {
      val result = translateSpec(spec)
      if (errors.isEmpty) Success(result, warnings)
      else Failure(errors, warnings)
    } catch {
      case ex: TesslaError =>
        Failure(errors += ex, warnings)
    }
  }
}

object TranslationPhase {
  sealed trait Result[+T] {
    val warnings: Seq[Diagnostic]
    def andThen[T2 >: T, U](f: TranslationPhase[T2, U]): Result[U]
    def map[U](f: T => U): Result[U]
    def foreach(f: T => Unit): Unit = map(f)
  }

  case class Success[+T](value: T, warnings: Seq[Diagnostic]) extends Result[T] {
    override def andThen[T2 >: T, U](t: TranslationPhase[T2, U]): Result[U] = t.translate(value) match {
      case Success(newValue, newWarnings) => Success(newValue, warnings ++ newWarnings)
      case Failure(errors, newWarnings) => Failure(errors, warnings ++ newWarnings)
    }

    override def map[U](f: T => U) = Success(f(value), warnings)
  }

  case class Failure(errors: Seq[TesslaError], warnings: Seq[Diagnostic]) extends Result[Nothing] {
    override def andThen[T2, U](f: TranslationPhase[T2, U]) = this

    override def map[U](f: Nothing => U) = this
  }

  case class SimpleWarning(loc: Location, message: String) extends Diagnostic
}