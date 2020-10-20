package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.AbstractTestRunner
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success
import spray.json._

class AnnotationJsonTests extends AbstractTestRunner[String]("Annotations-Json") {
  override def roots: Seq[String] = Seq("annotations-json/")
  override def translation: TranslationPhase[Core.Specification, String] =
    (spec: Core.Specification) => Success(AnnotationsToJson(spec), Seq())

  override def compareCompilerResult(actual: String, expected: String): Unit = {
    assert(actual.parseJson == expected.parseJson)
  }
}
