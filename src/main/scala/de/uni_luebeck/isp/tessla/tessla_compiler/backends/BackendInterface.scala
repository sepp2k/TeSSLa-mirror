package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.{IntermediateCodeTypeInference, IntermediateCodeUtils}

/**
 * Abstract base class for the translation from IntermediateCode to real source code
 * @param sourceTemplate Resource path of the template code, where generated code is inserted
 */
abstract class BackendInterface(sourceTemplate: String) extends TranslationPhase[SourceListing, String] {

  var variables: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)] = Map()

  def translate(orgListing: SourceListing): Result[String] = {
    var warnings = Seq()

    val nonStaticVars = IntermediateCodeUtils.getVariableMap(
      orgListing.tsGenSource
        .concat(orgListing.stepSource)
        .concat(orgListing.inputProcessing)
        .concat(orgListing.tailSource)
    ) ++
      Map(
        "inputStream" -> (StringType, scala.Some(StringValue("")), false),
        "value" -> (StringType, scala.Some(StringValue("")), false),
        "currTs" -> (LongType, scala.Some(LongValue(0)), false),
        "lastProcessedTs" -> (LongType, scala.Some(LongValue(0)), false),
        "newInputTs" -> (LongType, scala.Some(LongValue(0)), false)
      )

    val staticVars = IntermediateCodeUtils.getVariableMap(orgListing.staticSource)

    variables = nonStaticVars ++ staticVars

    val listing =
      IntermediateCodeTypeInference.generateCodeWithCasts(orgListing, variables.map { case (n, (t, _, _)) => (n, t) })

    val source = scala.io.Source.fromResource(sourceTemplate).mkString
    val rewrittenSource = source
      .replaceAllLiterally("//VARDEF", generateVariableDeclarations(nonStaticVars).mkString("\n"))
      .replaceAllLiterally("//TRIGGER", generateCode(listing.tsGenSource))
      .replaceAllLiterally("//STEP", generateCode(listing.stepSource))
      .replaceAllLiterally("//TAIL", generateCode(listing.tailSource))
      .replaceAllLiterally("//INPUTPROCESSING", generateCode(listing.inputProcessing))
      .replaceAllLiterally(
        "//STATIC",
        generateVariableDeclarations(staticVars).mkString("\n") + "\n\n" + generateCode(listing.staticSource)
      )

    Success(rewrittenSource, warnings)
  }

  def generateVariableDeclarations(vars: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)]): Seq[String]

  def generateCode(stmts: Seq[ImpLanStmt]): String
}
