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
trait TranslationPhase[-T, +U] extends (T=>Result[U]) {
  def translate(spec: T): Result[U]

  def apply(spec: T): Result[U] = translate(spec)

  def andThen[V](other: TranslationPhase[U, V]): TranslationPhase[T, V] = (spec: T) => {
    translate(spec).andThen(other)
  }
}

object TranslationPhase {
  abstract class Translator[U] {
    protected def translateSpec(): U

    def translate(): Result[U] = {
      try {
        val result = translateSpec()
        if (errors.isEmpty) Success(result, warnings)
        else Failure(errors, warnings)
      } catch {
        case ex: TesslaError =>
          Failure(errors += ex, warnings)
      }
    }

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
  }

  sealed trait Result[+T] {
    val warnings: Seq[Diagnostic]
    def andThen[U](f: T=>Result[U]): Result[U]
    def map[U](f: T => U): Result[U]
    def foreach(f: T => Unit): Unit = map(f)
  }

  case class Success[+T](value: T, warnings: Seq[Diagnostic]) extends Result[T] {
    override def andThen[U](t: T=>Result[U]): Result[U] = t(value) match {
      case Success(newValue, newWarnings) => Success(newValue, warnings ++ newWarnings)
      case Failure(errors, newWarnings) => Failure(errors, warnings ++ newWarnings)
    }

    override def map[U](f: T => U) = Success(f(value), warnings)
  }

  case class Failure(errors: Seq[TesslaError], warnings: Seq[Diagnostic]) extends Result[Nothing] {
    override def andThen[U](f: Nothing=>Result[U]) = this

    override def map[U](f: Nothing => U) = this
  }

  case class SimpleWarning(loc: Location, message: String) extends Diagnostic
}