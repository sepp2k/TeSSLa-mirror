package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.{IntermediateCodeTypeInference, IntermediateCodeUtils}

/**
 * Abstract base class for the translation from intermediate code to real source code
 * @param sourceTemplate Resource path of the template code, where generated code is inserted
 */
abstract class BackendInterface(sourceTemplate: String) extends TranslationPhase[SourceListing, String] {

  protected var variables: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)] = Map()

  /**
   * Function triggering the translation from a [[SourceListing]] to real-world source code as String.
   * Therefore the sections of the [[SourceListing]] are translated to code and included at special places of
   * the template code by replacing
   *
   * - //VARDEF by a section where all used variables are defined
   * - //TRIGGER by the translation of tsGenSource
   * - //STEP by the translation of stepSource
   * - //TAIL by the translation of tailSource
   * - //INPUTPROCESSING by the translation of inputProcessing
   * - //STATIC by the translation of staticSource
   *
   * For more details on the requirements of the template see the markdown documents in doc
   *
   * @param orgListing The source listing to be translated
   * @return The source code of the generated monitor program
   */
  def translate(orgListing: SourceListing): Result[String] = {
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

    Success(rewrittenSource, Seq())
  }

  /**
   * Translates a map of variables with base information (type, default value, lazy assignment) to corresponding
   * variable declarations in the target source code.
   * Has to be implemented by Backend-Implementations.
   * @param vars Map of variables to be translated: Name -> (Type x Default value x Lazy assignment)
   * @return Sequence of variable definitions
   */
  def generateVariableDeclarations(vars: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)]): Seq[String]

  /**
   * Translates a sequence of [[ImpLanStmt]] to the corresponding code in the target language.
   * Has to be implemented by Backend-Implementations.
   * @param stmts The sequence of statements to be translated.
   * @return The generated code in the target language
   */
  def generateCode(stmts: Seq[ImpLanStmt]): String
}
