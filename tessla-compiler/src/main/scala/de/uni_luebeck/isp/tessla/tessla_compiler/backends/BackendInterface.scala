package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.{Errors, IntermediateCodeTypeInference, IntermediateCodeUtils}

import scala.io.Source

/**
  * Abstract base class for the translation from IntermediateCode to real source code
  * @param sourceTemplate Resource path of the template code, where generated code is inserted
  * @param userInclude Path of user-specific code which is included in the generated code
  */
abstract class BackendInterface(userInclude: String, sourceTemplate: String) extends TranslationPhase[SourceListing, String] {

  var variables : Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)] = Map()

  def translate(orgListing: SourceListing) : Result[String] = {
    var warnings = Seq()

    val nonStaticVars = IntermediateCodeUtils.getVariableMap(orgListing.tsGenSource.concat(orgListing.stepSource).concat(orgListing.callbacks).concat(orgListing.tailSource), Map(),true) ++
      Map("inputStream" -> (StringType, scala.Some(StringValue("")), true),
      "value" -> (StringType, scala.Some(StringValue("")), true),
      "currTs" -> (LongType, scala.Some(LongValue(0)), true),
      "lastProcessedTs" -> (LongType, scala.Some(LongValue(0)), true))

    val staticVars = IntermediateCodeUtils.getVariableMap(orgListing.staticSource, Map(), true)

    variables = nonStaticVars ++ staticVars

    val listing = IntermediateCodeTypeInference.generateCodeWithCasts(orgListing, variables.map{case (n, (t, _, _)) => (n, t)})

    val source =  scala.io.Source.fromResource(sourceTemplate).mkString
    val userCode = if (userInclude != "") {
      val uc = Source.fromFile(userInclude)
      val cont = uc.getLines.mkString("\n")
      uc.close()
      cont
    } else {
      ""
    }

    val rewrittenSource = source
        .replaceAllLiterally("//VARDEF", generateVariableDeclarations(nonStaticVars).mkString("\n"))
        .replaceAllLiterally("//TRIGGER", generateCode(listing.tsGenSource))
        .replaceAllLiterally("//STEP", generateCode(listing.stepSource))
        .replaceAllLiterally("//TAIL", generateCode(listing.tailSource))
        .replaceAllLiterally("//CALLBACKS", generateCode(listing.callbacks))
        .replaceAllLiterally("//STATIC", generateVariableDeclarations(staticVars).mkString("\n") + "\n\n" + generateCode(listing.staticSource))
        .replaceAllLiterally("//USERCODE", userCode)

    Success(rewrittenSource, warnings)
  }

  def generateVariableDeclarations(vars: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)]) : Seq[String]

  def generateCode(stmts: Seq[ImpLanStmt]) : String
}
