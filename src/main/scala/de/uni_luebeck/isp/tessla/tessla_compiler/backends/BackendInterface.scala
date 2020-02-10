package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.{Errors, IntermediateCodeTypeInference, IntermediateCodeUtils}

/**
  * Abstract base class for the translation from IntermediateCode to real source code
  * @param sourceTemplate Resource path of the template code, where generated code is inserted
  */
abstract class BackendInterface(sourceTemplate: String) extends TranslationPhase[SourceListing, String] {

  var variables : Map[String, (ImpLanType, Option[ImpLanExpr])] = Map()

  def translate(orgListing: SourceListing) : Result[String] = {
    var warnings = Seq()

    val nonStaticVars = IntermediateCodeUtils.getVariableMap(orgListing.tsGenSource.concat(orgListing.stepSource).concat(orgListing.inputProcessing)) ++
      Map("inputStream" -> (StringType, scala.Some(StringValue(""))),
      "value" -> (StringType, scala.Some(StringValue(""))),
      "currTs" -> (LongType, scala.Some(LongValue(0))))

    val staticVars = IntermediateCodeUtils.getVariableMap(orgListing.staticSource)

    variables = nonStaticVars ++ staticVars

    val listing = IntermediateCodeTypeInference.generateCodeWithCasts(orgListing, variables.map{case (n, (t, _)) => (n, t)})

    val source =  scala.io.Source.fromResource(sourceTemplate).mkString
    val rewrittenSource = source.replaceAllLiterally("//VARDEF", generateVariableDeclarations(nonStaticVars).mkString("\n"))
        .replaceAllLiterally("//TRIGGER", generateCode(listing.tsGenSource))
        .replaceAllLiterally("//STEP", generateCode(listing.stepSource))
        .replaceAllLiterally("//INPUTPROCESSING", generateCode(listing.inputProcessing))
        .replaceAllLiterally("//STATIC", generateVariableDeclarations(staticVars).mkString("\n") + "\n\n" + generateCode(listing.staticSource))

    Success(rewrittenSource, warnings)
  }

  def generateVariableDeclarations(vars: Map[String, (ImpLanType, Option[ImpLanExpr])]) : Seq[String]

  def generateCode(stmts: Seq[ImpLanStmt]) : String
}
