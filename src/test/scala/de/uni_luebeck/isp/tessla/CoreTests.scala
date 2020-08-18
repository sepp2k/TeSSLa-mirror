package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}

class CoreTests extends AbstractTestRunner[Core.Specification]("Core") {
  override def roots: Seq[String] = Seq("core/")
  override def translation: TranslationPhase[Core.Specification, Core.Specification] = TranslationPhase.IdentityPhase()
}
