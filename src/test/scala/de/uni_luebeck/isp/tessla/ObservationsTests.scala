package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.core.analyses.Observations
import spray.json.JsonParser

class ObservationsTests extends AbstractTestRunner[Observations]("Observations") {
  override def roots: Seq[String] = Seq("observations/")

  override def translation: TranslationPhase[Core.Specification, Observations] = Observations.Generator

  override def compareCompilerResult(compilerResult: Observations, expectedResult: String): Unit = {
    val expectedObservation = JsonParser(expectedResult).convertTo[Observations]
    assert(compilerResult === expectedObservation)
  }
}
