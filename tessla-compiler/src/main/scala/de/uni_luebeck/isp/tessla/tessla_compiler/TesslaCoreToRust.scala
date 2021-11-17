package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success, Translator}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.SourceListing

import scala.annotation.tailrec

/**
 * Class implementing TranslationPhase for the translation from TeSSLa Core to
 * rust code
 * The translation of stream functions is performed in StreamCodeGenerator TODO
 * The translation of other expressions in NonStreamCodeGenerator TODO
 * @param ioInterface Indicates whether the generated code shall be able to read/write from/to stdio
 */
class TesslaCoreToRust(ioInterface: Boolean) extends TranslationPhase[ExtendedSpecification, String] {
  override def translate(spec: ExtendedSpecification): Result[String] = ???
}
