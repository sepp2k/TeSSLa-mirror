package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.AbstractTestRunner
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success
import org.scalactic.Prettifier
import spray.json._

class AnnotationJsonTests extends AbstractTestRunner[String]("Annotations-Json") {
  override def roots: Seq[String] = Seq("annotations-json/")
  override def translation: TranslationPhase[Core.Specification, String] =
    (spec: Core.Specification) => Success(AnnotationsToJson(spec), Seq())

  implicit val prettifier: Prettifier = {
    case js: JsValue => js.prettyPrint
    case o           => Prettifier.default(o)
  }

  override def compareCompilerResult(actual: String, expected: String): Unit = {
    val Replace = """"name"\s*:\s*"(.*)\$\d+""""
    val a = actual.replaceAll(Replace, """"name": "$1"""")
    val e = expected.replaceAll(Replace, """"name": "$1"""")

    assert(a.parseJson == e.parseJson)
  }
}
