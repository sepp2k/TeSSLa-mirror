package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TesslaAST.{LazyEvaluation, StrictEvaluation}
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success

import java.util
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.util.control.Breaks._

/**
 * This object is part of the translation pipeline of the Rust backend. The idea behind this is that format strings must
 * be known in Rust at compile time. This translation stage therefore looks in the ExtendedSpecification from the
 * TeSSLa core for all String.format() calls and removes the default stream and its corresponding nil stream generated by
 * the TeSSLa core. In addition, this translation stage translates the format string into a Rust format string and
 * replaces the String.format() call.
 */
object FormatStringMangler
    extends TranslationPhase[Specification, Specification] {
  override def translate(spec: Specification): TranslationPhase.Result[Specification] = {
    val removedStreams = new util.ArrayList[String]()
    val formatStrings = new mutable.HashMap[String, String]()

    // Find every slift that uses extern("String_format")
    spec.definitions.foreach {
      case (sliftid, definition) =>
        definition match {
          case ApplicationExpression(TypeApplicationExpression(ExternExpression("slift", _, _), _, _), args, _)
              if args.length >= 3 =>
            args(2) match {
              case TypeApplicationExpression(ExternExpression("String_format", _, loc), _, _) =>
                args(0) match {
                  case ExpressionRef(id, _, _) =>
                    // If we encounter this format string the first time
                    if (!removedStreams.contains(id.fullName)) {
                      spec.definitions.get(id) match {
                        // Ensure that the format string is stored on a nil stream using default
                        case Some(
                              ApplicationExpression(
                                TypeApplicationExpression(ExternExpression("default", _, _), _, _),
                                args,
                                _
                              )
                            ) =>
                          val stream = args(0) match { // the stream the format string is based on
                            case ExpressionRef(id, _, _) => id
                            case _ =>
                              throw Diagnostics.CommandNotSupportedError(
                                "Can't determine format string at compile time.",
                                loc
                              )
                          }

                          // Do nothing if the format string is based on a nil stream
                          spec.definitions.get(stream) match {
                            case Some(
                                  ApplicationExpression(
                                    TypeApplicationExpression(ExternExpression("nil", _, _), _, _),
                                    _,
                                    _
                                  )
                                ) => {}
                            case _ =>
                              throw Diagnostics.CommandNotSupportedError(
                                "Can't determine format string at compile time.",
                                loc
                              )
                          }

                          // Remove the nil stream
                          removedStreams.add(stream.fullName)

                          var format = args(1) match { // the format string itself
                            case StringLiteralExpression(value, _) => value
                            case _ =>
                              throw Diagnostics.CommandNotSupportedError(
                                "Can't determine format string at compile time.",
                                loc
                              )
                          }

                          // Add the format string to the map
                          formatStrings.addOne((sliftid.fullName, format))

                          // Remove the format string stream
                          removedStreams.add(id.fullName)
                        case _ =>
                          throw Diagnostics.CommandNotSupportedError(
                            "Can't determine format string at compile time.",
                            loc
                          )
                      }
                    }

                    // We encountered the format string already and removed the streams
                    else
                      /* we just need to map the slift to the corresponding format string */ {
                        formatStrings.addOne((sliftid.fullName, formatStrings(id.fullName)))
                      }
                  case _ => {}
                }
              case _ => {}
            }

          case _ => {}
        }
    }

    Success(modifyFormatStringSlifts(removeUnusedStreams(spec, removedStreams), formatStrings), Seq())
  }

  /**
   * Removes streams that became obsolete due to the format string mangling.
   *
   * @param spec The specification to modify.
   * @param streamsToRemove The name of the streams to remove.
   * @return The modified specification without the specified streams.
   */
  def removeUnusedStreams(spec: Specification, streamsToRemove: util.ArrayList[String]): Specification = {
    val definitions = spec.definitions.filterNot {
      case (id, _) => streamsToRemove.contains(id.fullName)
    }

    Specification(spec.annotations, spec.in, definitions, spec.out, spec.maxIdentifier)
  }

  /**
   * Morphs the slift2(String, Value, extern("String_format")) calls into slift1(Value, ...) calls that contain a lambda
   * which uses Rusts format! macro to format the string.
   *
   * @param spec The specification to modify.
   * @param formatStrings The format string streams and their corresponding format strings.
   * @return The modified specification.
   */
  def modifyFormatStringSlifts(spec: Specification, formatStrings: mutable.HashMap[String, String]): Specification = {
    System.out.println("")

    val definitions = spec.definitions.map {
      case (id, defi) if formatStrings.contains(id.fullName) => {
        val location = defi.location
        var inputStream : ExpressionArg = null
        var inputStreamType : Type = null

        defi match {
          case ApplicationExpression(applicable, args, _) => {
            inputStream = args(1)

            applicable match {
              case TypeApplicationExpression(_, args, _) => {
                inputStreamType = args(1)
              }
            }
          }
        }

        val fs_spec = parseFormatString(formatStrings.get(id.fullName).get)
        val fstring = produceRustFormatString(fs_spec)

        (id, ApplicationExpression(
          ExternExpression("slift",
            FunctionType(
              List(),                 // Identifier
              List(                   // Parameter
                (StrictEvaluation, inputStreamType),
                (StrictEvaluation, FunctionType(List(), List((StrictEvaluation, inputStreamType)), InstantiatedType("String", List())))
              ),
              InstantiatedType("Events", List(InstantiatedType("String", List())))    // Return type
            )),
          ArraySeq( // slift arguments: input stream and lambda that calls format!
            inputStream,
            FunctionExpression(List(), List((Identifier("x"), StrictEvaluation, inputStreamType)), Map(),
              (fs_spec.padSign, fs_spec.formatType == 'g') match {
                /* neither sign padding (' ') nor %g
                    => simply call format!
                 */
                case (false, false) => generateNoSignNoG(fstring, inputStreamType)

                /* %g
                  if 10e-4_f64 <= fabs(value) && fabs(value) < 10_f64.powi(precision) {
                    format_to_type_f(modified_spec, value)
                  } else {
                    format_to_type_e(modified_spec, value)
                  }
                 */
                case (false, true) => generateNoSignG(fstring, inputStreamType, fs_spec)

                /* sign padding (' ')
                  if value < 0 {
                    format!("{...}")
                  } else {
                    format(" {...}")
                  }
                 */
                case (true, false) => {
                  ???
                }
                case (true, true) => {
                  ???
                }
              }
            ))
        ))
      }
      // TODO: Rust: padSign, upper case strings, %g
      /**
       * -padSign, -%g
       * format!()
       *
       * -padSign, %g
       * s.u.
       *
       * padSign, -%g
       * s.u.
       *
       * padSign, %g
       * reinkopieren
       *
       * if x >= 0 {
       *    format!(" {}", ...)
       * }
       *
       *
       */
      case other => other
    }

    Specification(spec.annotations, spec.in, definitions, spec.out, spec.maxIdentifier)
  }

  /**
   * Generates a format! call for a format string that specifies neither sign padding nor %g.
   *
   * @param fstring The format string.
   * @param inputStreamType The type of the input stream.
   * @return The format! call.
   */
  def generateNoSignNoG(fstring : String, inputStreamType : Type) = {
    ApplicationExpression(ExternExpression("[rust]format",
      FunctionType(   // The lambda
        List(),       // Identifier
        List(         // Parameter
          (StrictEvaluation, InstantiatedType("String", List())),
          (StrictEvaluation, inputStreamType)),
        InstantiatedType("String", List())    // Return type
      )),
      ArraySeq(StringLiteralExpression(fstring), ExpressionRef(Identifier("x"), inputStreamType))
    )
  }

  /**
   * Generates a format! call for a format string that doesn't specify sign paddig but %g.
   *
   * @param fstring The format string.
   * @param inputStreamType The type of the input stream.
   * @param fs_spec The format string specification.
   * @return The format! call.
   */
  def generateNoSignG(fstring : String, inputStreamType : Type, fs_spec : FormatStringSpecification) = {
    ApplicationExpression(ExternExpression("ite",
      FunctionType(   // The if
        List(),       // Identifier
        List(         // Parameter
          (StrictEvaluation, InstantiatedType("Bool", List())),
          (LazyEvaluation, InstantiatedType("String", List())),
          (LazyEvaluation, InstantiatedType("String", List()))
        ),
        InstantiatedType("String", List())    // Return type
      )),
      ArraySeq(   // Arguments of the if: (exp : bool, then : T, else : T)
        ApplicationExpression(ExternExpression("and",   // &&
          FunctionType(
            List(),                   // Identifier
            List(                     // Parameter
              (LazyEvaluation, InstantiatedType("Bool", List())),
              (LazyEvaluation, InstantiatedType("Bool", List()))
            ),
            InstantiatedType("Bool", List())  // Return type
          )),
          ArraySeq(    // Arguments of the &&: (op1 : bool, op2 : bool)
            ApplicationExpression(ExternExpression("fleq",  // 10e-4_f64 <= value
              FunctionType(
                List(),
                List(
                  (StrictEvaluation, InstantiatedType("Float", List())),
                  (StrictEvaluation, InstantiatedType("Float", List()))
                ),
                InstantiatedType("Bool", List())
              )),
              ArraySeq(   // Arguments of fleq (<=)
                FloatLiteralExpression(10e-4),
                ApplicationExpression(ExternExpression("fabs",    // abs(value)
                  FunctionType(
                    List(),
                    List(
                      (StrictEvaluation, InstantiatedType("Float", List()))
                    ),
                    InstantiatedType("Float", List())
                  )
                ),
                  ArraySeq(
                    ExpressionRef(Identifier("x"), inputStreamType)
                  )
                )     // end of abs(value)
              )
            ),
            ApplicationExpression(ExternExpression("flt",  // value < 10_f64.powi(precision)
              FunctionType(
                List(),
                List(
                  (StrictEvaluation, InstantiatedType("Float", List())),
                  (StrictEvaluation, InstantiatedType("Float", List()))
                ),
                InstantiatedType("Bool", List())
              )),
              ArraySeq(   // Arguments of flt (<)
                ApplicationExpression(ExternExpression("fabs",    // abs(value)
                  FunctionType(
                    List(),
                    List(
                      (StrictEvaluation, InstantiatedType("Float", List()))
                    ),
                    InstantiatedType("Float", List())
                  )
                ),
                  ArraySeq(
                    ExpressionRef(Identifier("x"), inputStreamType)
                  )
                ),     // end of abs(value)
                FloatLiteralExpression(Math.pow(10, fs_spec.precision))
              )
            )
          )     // end of && arguments
        ),
        ApplicationExpression(ExternExpression("[rust]format",    // format_to_type_f
          FunctionType(   // The lambda
            List(),       // Identifier
            List(         // Parameter
              (StrictEvaluation, InstantiatedType("String", List())),
              (StrictEvaluation, inputStreamType)),
            InstantiatedType("String", List())    // Return type
          )),
          ArraySeq(StringLiteralExpression(fstring), ExpressionRef(Identifier("x"), inputStreamType))
        ),
        ApplicationExpression(ExternExpression("[rust]format",  // format_to_type_e
          FunctionType(   // The lambda
            List(),       // Identifier
            List(         // Parameter
              (StrictEvaluation, InstantiatedType("String", List())),
              (StrictEvaluation, inputStreamType)),
            InstantiatedType("String", List())    // Return type
          )),
          ArraySeq(StringLiteralExpression(fstring.replace("}", "e}")), ExpressionRef(Identifier("x"), inputStreamType))
        )
      )
    )
  }

  def generateSignPaddingNoG() = {

  }

  def generateSignPaddingG() = {

  }

  /**
   * Parses the specified TeSSLa format string.
   *
   * @param fs The format string to parse.
   * @return The extracted format string specification.
   */
  def parseFormatString(fs: String): FormatStringSpecification = {
    var spec: FormatStringSpecification = new FormatStringSpecification()

    if (fs.length < 2) {
      throw Diagnostics.CommandNotSupportedError("Invalid format string.")
    }
    if (fs.charAt(0) != '%') {
      throw Diagnostics.CommandNotSupportedError("Format string does not start with '%'.")
    }

    var i = 1

    // Extract flags; each only allowed once
    breakable {
      while (true) {
        fs.charAt(i) match {
          case '-' =>
            if (!spec.leftJustify) {
              spec.leftJustify = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string.")
            }
          case '+' =>
            if (!spec.plusSign) {
              spec.plusSign = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string.")
            }
          case ' ' =>
            if (!spec.padSign) {
              spec.padSign = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string.")
            }
          case '#' =>
            if (!spec.altForm) {
              spec.altForm = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string.")
            }
          case '0' =>
            if (!spec.zeroPad) {
              spec.zeroPad = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string.")
            }
          case ',' =>
            if (!spec.localeSeparators) {
              spec.localeSeparators = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string.")
            }
          case '(' =>
            if (!spec.encloseNegatives) {
              spec.encloseNegatives = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string.")
            }
          case _ => break
        }

        i += 1
      }
    }

    try {
      // Extract width, don't allow zeroes
      if (fs.charAt(i) != '0' && fs.charAt(i).isDigit) {
        val j = i;

        while (fs.charAt(i).isDigit) {
          i += 1
        }

        spec.width = Integer.parseInt(fs.substring(j, i))
      }

      // Extract precision
      if (fs.charAt(i) == '.') {
        i+=1
        val j = i

        while (fs.charAt(i).isDigit) {
          i += 1
        }

        spec.precision = Integer.parseInt(fs.substring(j, i))
      }

      // Extract the format specifier
      spec.formatType = fs.charAt(i)

      // Determine whether the output should be in upper case
      if (spec.formatType.isUpper) {
        spec.uppercase = true
        spec.formatType = spec.formatType.toLower
      }
    } catch {
      case e: Exception => throw Diagnostics.CommandNotSupportedError("Invalid format string: " + e.getMessage)
    }

    if (i + 1 != fs.length) {
      throw Diagnostics.CommandNotSupportedError("Invalid format string.")
    }

    spec
  }

  /**
   * Specifies a format string.
   */
  class FormatStringSpecification {

    /**
     * Left justify withing the given field of width FmtWidth.
     */
    var leftJustify = false

    /**
     * Precede the output with a '+', unless the value is negative.
     */
    var plusSign = false

    /**
     * If no sign is written, write a space (0x20) instead.
     */
    var padSign = false

    /**
     * Precede integer with 0 for o, 0x for x and 0X for X.
     */
    var altForm = false

    /**
     * Left-pad the number with 0 if a padding is specified.
     */
    var zeroPad = false

    /**
     * Use a locale-specific grouping separator.
     */
    var localeSeparators = false

    /**
     * Enclose negative numbers with parenthesis.
     */
    var encloseNegatives = false

    /**
     * A non-negative decimal integer specifying the minimal number
     * of characters written to the output.
     */
    var width = 0

    /**
     * The maximum numbers of characters written for floating points.
     * Not applicable to o, d, x and X types.
     */
    var precision = 0

    /**
     * Indicates whether the output should be in upper case.
     */
    var uppercase = false

    /**
     * The format specifier.
     */
    var formatType = '?'

    /**
     * Determines whether this format specifier specifies an integer format.
     *
     */
    def isInteger() = {
      formatType == 'x' || formatType == 'o' || formatType == 'd'
    }

    /**
     * Determines whether this format specifier specifies a floating point format.
     */
    def isFloat() = {
      formatType == 'f' || formatType == 'g' || formatType == 'e' || formatType == 'a'
    }

    /**
     * Determines whether this format specifies neither an integer nor a floating
     * point format.
     */
    def isOther() = {
      !isFloat() && !isInteger()
    }
  }

  /**
   * Translates the specified format string into a format string
   * that can be fed into Rusts format!-macro.
   *
   * @param fs The format string to translate.
   * @return A format string for Rusts format!-macro.
   */
  def produceRustFormatString(fs: FormatStringSpecification): String = {
    if (fs.formatType == 'a') {
      throw Diagnostics.CommandNotSupportedError(
        "Hexadecimal floating point literals aren't supported in Rust format strings."
      )
    }
    if (fs.encloseNegatives) {
      throw Diagnostics.CommandNotSupportedError("Enclosing negative numbers is not supported in Rust format strings.")
    }
    if (fs.localeSeparators) {
      throw Diagnostics.CommandNotSupportedError(
        "Locale-specific grouping separators aren't supported in Rust format strings."
      )
    }
    if (fs.isOther && fs.zeroPad) {
      throw Diagnostics.CommandNotSupportedError("Strings can be zero-padded.")
    }
    if (fs.precision > 0 && fs.isInteger) {
      throw Diagnostics.CommandNotSupportedError("Integer formats can't use a precision")
    }
    if (fs.padSign && fs.plusSign) {
      throw Diagnostics.CommandNotSupportedError("Don't specify + and space.")
    }
    if (fs.altForm && !fs.isInteger()) {
      throw Diagnostics.CommandNotSupportedError("Alternate form can't be used with non-integer types.")
    }

    // Apply format flags
    var str = "{:"
    if (fs.width > 0) {
      if (fs.leftJustify) {
        str += '<'
      } else {
        str += '>'
      }
    }
    if (fs.altForm) {
      str += '#'
    }
    if (fs.plusSign) {
      str += '+'
    }
    if (fs.zeroPad) {
      str += '0'
    }
    if (fs.width > 0) {
      str += fs.width;
    }
    if (fs.precision > 0) {
      str += '.'
      str += fs.precision
    }

    // Add format specifier
    if (fs.formatType == 'x') {
      str += (if (fs.uppercase) { 'X' }
              else { 'x' })
    }
    if (fs.formatType == 's' || fs.formatType == 'd' || fs.formatType == 'f' || fs.formatType == 'g') {
      // Add nothing
    }
    if (fs.formatType == 'o') {
      str += 'o'
    }
    if (fs.formatType == 'e') {
      str += 'e'
    }

    str += '}'
    str
  }
}
