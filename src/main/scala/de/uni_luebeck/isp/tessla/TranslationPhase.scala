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
  trait Translator[U] {
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

    protected def unwrapResult[T](result: Result[T]): T = result match {
      case Success(value, resultWarnings) =>
        warnings ++= resultWarnings
        value
      case Failure(resultErrors, resultWarnings) =>
        warnings ++= resultWarnings
        errors ++= resultErrors.view.init
        throw resultErrors.last
    }
  }

  sealed trait Result[+T] {
    val warnings: Seq[Diagnostic]
    def andThen[U](f: T=>Result[U]): Result[U]

    /**
      * Combine two results, such that the new result is a Success(f(x,y), ws1++ws2) if
      * both results are successes (with the values x and y and the warnings ws1 and ws2 respectively),
      * or a Failure containing both results' failures and warnings if at least one of the results is
      * a failure.
      */
    def combine[U, V](other: Result[U])(f: (T, U) => V): Result[V]
    def map[U](f: T => U): Result[U]
    def foreach(f: T => Unit): Unit = map(f)
  }

  object Result {
    /**
      * Apply the function `f` to the elements of `xs` until it returns a `Failure` or until
      * the end of the list is reached. If a `Failure` was returned, the result is also a failure
      * with the same error messages and all the warnings that have been produced so far.
      * If all results are `Success`es, the end result is a `Success` containing a list of the produced
      * values and all the warnings produced.
      * Note that if a Failure is produced, `f` will not be called on any later elements, so no warnings
      * or errors that would be produced by those elements, will appear in the result.
      */
    def runSequentially[T, U](xs: Iterable[T])(f: T => Result[U]): Result[Seq[U]] = {
      if (xs.isEmpty) Success(Seq(), Seq())
      else f(xs.head).andThen(x => runSequentially(xs.tail)(f).map(xs => x +: xs))
    }

    /**
      * Combines all the results in the given sequence into a single result. If all the results in the
      * sequence are `Success`es, the end result will be a `Success` containing a sequence of their values
      * and all their warnings (both in the original order of the sequence). Otherwise, a `Failure` will be
      * returned that contains all errors and all warnings contained in any of the results (again maintaing
      * the order of the original sequence).
      */
    def combineAll[T](results: Iterable[Result[T]]): Result[Seq[T]] = {
      val empty: Result[Seq[T]] = Success(Seq(), Seq())
      results.foldRight(empty) { (result, acc) =>
        result.combine(acc) { (item, items) =>
          item +: items
        }
      }
    }
  }

  case class Success[+T](value: T, warnings: Seq[Diagnostic]) extends Result[T] {
    override def andThen[U](t: T=>Result[U]): Result[U] = t(value) match {
      case Success(newValue, newWarnings) => Success(newValue, warnings ++ newWarnings)
      case Failure(errors, newWarnings) => Failure(errors, warnings ++ newWarnings)
    }

    override def combine[U, V](other: Result[U])(f: (T, U) => V) = {
      andThen(_ => other.map(otherValue => f(value, otherValue)))
    }

    override def map[U](f: T => U) = Success(f(value), warnings)
  }

  case class Failure(errors: Seq[TesslaError], warnings: Seq[Diagnostic]) extends Result[Nothing] {
    override def andThen[U](f: Nothing=>Result[U]) = this

    override def map[U](f: Nothing => U) = this

    override def combine[U, V](other: Result[U])(f: (Nothing, U) => V) = other match {
      case Success(_, otherWarnings) => Failure(errors, warnings ++ otherWarnings)
      case Failure(otherErrors, otherWarnings) => Failure(errors ++ otherErrors, warnings ++ otherWarnings)
    }
  }

  case class SimpleWarning(loc: Location, message: String) extends Diagnostic
}