package de.uni_luebeck.isp.tessla

import TranslationPhase._
import de.uni_luebeck.isp.tessla.Errors.TesslaError

import scala.collection.mutable.ArrayBuffer

/**
  * Trait representing a phase of the translation process.
  *
  * Note that this trait contains mutable members and should only be extended by classes, not objects.
  *
  * @tparam T The type of representation that this phase will be applied to
  * @tparam U The result of applying this phase
  */
trait TranslationPhase[T, U] {
  def translateSpec(spec: T): U

  val warnings = ArrayBuffer[Diagnostic]()
  val errors = ArrayBuffer[TesslaError]()

  def warn(diagnostic: Diagnostic): Unit = {
    warnings += diagnostic
  }

  def error(error: TesslaError) {
    errors += error
  }

  def warn(loc: Location, message: String): Unit = warn(SimpleWarning(loc, message))

  def tryWithDefault[R](default: => R)(body: => R): R = {
    try {
      body
    } catch {
      case ex: TesslaError =>
        errors += ex
        default
    }
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
    def warnings: Seq[Diagnostic]
    def andThen[T2 >: T, U](f: TranslationPhase[T2, U]): Result[U]
  }
  case class Success[+T](value: T, warnings: Seq[Diagnostic]) extends Result[T] {
    override def andThen[T2 >: T, U](t: TranslationPhase[T2, U]): Result[U] = t.translate(value) match {
      case Success(newValue, newWarnings) => Success(newValue, warnings ++ newWarnings)
      case Failure(errors, newWarnings) => Failure(errors, warnings ++ newWarnings)
    }
  }
  case class Failure(errors: Seq[Diagnostic], warnings: Seq[Diagnostic]) extends Result[Nothing] {
    override def andThen[T2, U](f: TranslationPhase[T2, U]) = this
  }

  case class SimpleWarning(loc: Location, message: String) extends Diagnostic
}