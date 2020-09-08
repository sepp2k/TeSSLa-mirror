package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.instrumenter.CInstrumentation
import spray.json.JsonParser

class CInstrumentationTests extends AbstractTestRunner[Core.Specification]("Observations") {
  override def roots: Seq[String] = Seq()

  override def translation: TranslationPhase[Core.Specification, Core.Specification] = TranslationPhase.IdentityPhase()

}
