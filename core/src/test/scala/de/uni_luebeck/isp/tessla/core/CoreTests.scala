package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.AbstractTestRunner
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core

class CoreTests extends AbstractTestRunner[Core.Specification]("Core") {
  override def roots: Seq[String] = Seq("core/")
  override def translation: TranslationPhase[Core.Specification, Core.Specification] = TranslationPhase.IdentityPhase()
}
