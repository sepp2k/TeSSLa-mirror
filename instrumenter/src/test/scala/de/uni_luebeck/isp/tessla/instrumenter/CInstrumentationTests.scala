package de.uni_luebeck.isp.tessla.instrumenter

import de.uni_luebeck.isp.tessla.TestCase.{PathResolver, TestConfig}
import de.uni_luebeck.isp.tessla.{AbstractTestRunner, TestCase}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}

class CInstrumentationTests extends AbstractTestRunner[Core.Specification]("CInstrumentation") {

  override def roots: Seq[String] = Seq("instrumenter/")

  override def translation: TranslationPhase[Core.Specification, Core.Specification] = TranslationPhase.IdentityPhase()

  override def shouldIgnoreTest(t: (String, (String, String))): Boolean = !CInstrumentationBridge.isPlatformSupported
}
