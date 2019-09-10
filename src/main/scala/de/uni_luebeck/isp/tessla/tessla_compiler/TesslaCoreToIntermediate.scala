package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.{TesslaCore, TranslationPhase}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}

/**
  * This class is the de.uni_luebeck.isp.tessla.TranslationPhase for the translation from TeSSLa Core to
  * abstract imperative code
  */
object TesslaCoreToIntermediate extends
        TranslationPhase[TesslaCore.Specification, SourceListing] {


  override def translate(spec: TesslaCore.Specification): Result[IntermediateCode.SourceListing] = {
    Success(IntermediateCode.SourceListing(Seq()), Seq())
  }

}
