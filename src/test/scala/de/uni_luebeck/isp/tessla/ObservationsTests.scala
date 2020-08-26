package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.analyses.Observations
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import spray.json.JsonParser

class ObservationsTests extends AbstractTestRunner[Core.Specification]("Observations") {
  override def roots: Seq[String] = Seq()

  override def translation: TranslationPhase[Core.Specification, Core.Specification] = TranslationPhase.IdentityPhase()

}
