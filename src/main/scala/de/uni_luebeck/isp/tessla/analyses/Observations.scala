package de.uni_luebeck.isp.tessla.analyses

import de.uni_luebeck.isp.tessla.{Tessla, TesslaCore, TranslationPhase}
import play.api.libs.json.{Format, Json}
import Observations._

case class Observations(FunctionCalled: Seq[FunctionCalled] = Seq(),
                        FunctionReturns: Seq[FunctionReturns] = Seq()) {
  override def toString: String = {
    Json.prettyPrint(Json.toJson(this))
  }
}

object Observations {
  case class FunctionCalled(FunctionName: String, code: String)
  case class FunctionReturns(FunctionName: String, code: String)

  implicit val functionCalledFormat: Format[FunctionCalled] = Json.format[FunctionCalled]
  implicit val functionReturnsFormat: Format[FunctionReturns] = Json.format[FunctionReturns]
  implicit val observationsFormat: Format[Observations] = Json.format[Observations]

  class Generator(spec: TesslaCore.Specification) extends TranslationPhase.Translator[Observations] {
    override protected def translateSpec() = {
      val functionCalled = spec.inStreams.flatMap { in =>
        in.annotations.filter(_.name == "FunctionCalled").map { annotation =>
          val name = annotation.arguments("name") match {
            case Tessla.ConstantExpression.Literal(Tessla.StringLiteral(x), _) => x
            case _ => throw new InternalError("Name must be a string, should have been caught by the (not yet implemented) type checker.")
          }
          FunctionCalled(FunctionName = name,
            code = s"""fprintf(trace_outfile, "%lu: ${in.name}\\n", trace_get_normalized_timestamp());\\nfflush(trace_outfile);""")
        }
      }

      val setup = FunctionCalled("main", code = """trace_setup();""")
      val teardown = FunctionReturns("main", code = """trace_teardown();""")

      Observations(FunctionCalled = setup +: functionCalled, FunctionReturns = Seq(teardown))
    }
  }

  object Generator extends TranslationPhase[TesslaCore.Specification, Observations] {
    override def translate(spec: TesslaCore.Specification) = {
      new Generator(spec).translate()
    }
  }
}
