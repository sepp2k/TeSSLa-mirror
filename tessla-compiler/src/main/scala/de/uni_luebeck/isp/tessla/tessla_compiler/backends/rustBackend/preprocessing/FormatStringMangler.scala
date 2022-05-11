/*
 * Copyright 2022 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.preprocessing

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TesslaAST.{LazyEvaluation, StrictEvaluation}
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.core.{Location, TranslationPhase}
import de.uni_luebeck.isp.tessla.tessla_compiler.Diagnostics

import java.util
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.language.postfixOps
import scala.util.control.Breaks._

/**
 * This object is part of the translation pipeline of the Rust backend. The idea behind this is that format strings must
 * be known in Rust at compile time. This translation stage therefore looks in the ExtendedSpecification from the
 * TeSSLa core for all String.format() calls and removes the default stream and its corresponding nil stream generated by
 * the TeSSLa core. In addition, this translation stage translates the format string into a Rust format string and
 * replaces the String.format() call.
 */
object FormatStringMangler extends TranslationPhase[Specification, Specification] {
  override def translate(spec: Specification): TranslationPhase.Result[Specification] = {
    val removedStreams = new util.ArrayList[String]()
    val formatStrings = new mutable.HashMap[String, (String, Location)]()

    Success(
      modifyFormatStringSlifts(
        removeUnusedStreams(
          Specification(
            spec.annotations,
            spec.in,
            traverseRecursively(spec.definitions, removedStreams, formatStrings),
            spec.out,
            spec.maxIdentifier
          ),
          removedStreams
        ),
        formatStrings
      ),
      Seq()
    )
  }

  /**
   * Traverses recursively through the specified definitions and
   *   1. modifies direct calls to extern("String_format").
   *   2. gather data from calls to String.format which are wrapped in a slift2 in order to modify those in the following
   *      steps.
   * @param definitions The definitions to traverse.
   * @param removedStreams The name of the streams to remove.
   * @param formatStrings The format strings to translate.
   * @return The modified definition expressions.
   */
  def traverseRecursively(
    definitions: Map[Identifier, DefinitionExpression],
    removedStreams: util.ArrayList[String],
    formatStrings: mutable.HashMap[String, (String, Location)]
  ): Map[Identifier, DefinitionExpression] = {

    definitions.map {
      case (expid, defn) =>
        defn match {
          case FunctionExpression(t, p, b, r, l) =>
            expid -> FunctionExpression(t, p, traverseRecursively(b, removedStreams, formatStrings), r, l)
          // Find every slift that uses extern("String_format"); those get transformed seperately
          case ApplicationExpression(TypeApplicationExpression(ExternExpression("slift", _, loc), _, _), args, _)
              if args.length >= 3 =>
            args(2) match {
              case TypeApplicationExpression(ExternExpression("String_format", _, _), _, _) |
                  ExternExpression("String_format", _, _) =>
                args(0) match {
                  case ExpressionRef(id, _, _) =>
                    // If we encounter this format string the first time
                    //if (!removedStreams.contains(id.fullName)) {
                    definitions.get(id) match {
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
                        definitions.get(stream) match {
                          case Some(
                                ApplicationExpression(
                                  TypeApplicationExpression(ExternExpression("nil", _, _), _, _),
                                  _,
                                  _
                                )
                              ) =>
                          case _ =>
                            throw Diagnostics.CommandNotSupportedError(
                              "Can't determine format string at compile time.",
                              loc
                            )
                        }

                        val format = args(1) match { // the format string itself
                          case StringLiteralExpression(value, _) => value
                          case _ =>
                            throw Diagnostics.CommandNotSupportedError(
                              "Can't determine format string at compile time.",
                              loc
                            )
                        }

                        // Add the format string to the map
                        formatStrings.addOne((expid.fullName, (format, loc)))

                        // Remove the format string stream
                        removedStreams.add(id.fullName)
                        expid -> defn
                      case _ =>
                        throw Diagnostics.CommandNotSupportedError(
                          "Can't determine format string at compile time.",
                          loc
                        )
                    }
                  case _ => expid -> defn
                }
              case _ => expid -> defn
            }
          // Find dangling extern("String_format")s
          case ApplicationExpression(TypeApplicationExpression(applicable, typeArgs, loc), args, _) => {
            applicable match {
              case ExternExpression(n, t, l) => {
                if (n == "String_format") {
                  if (!args(0).isInstanceOf[StringLiteralExpression])
                    throw Diagnostics.DSLError("Expcted a string literal expression", loc)
                  val fs = args(0).asInstanceOf[StringLiteralExpression].value
                  val fs_spec = parseFormatString(fs, loc)
                  val fstring = produceRustFormatString(fs_spec, fs, loc)

                  var fn_format = (fs_spec.padSign, fs_spec.formatType == 'g') match {

                    /* The following four methods generate funky format string code because Rusts format! macro supports
                     * neither sign padding nor the %g format specifier.
                     */

                    /* neither sign padding (' ') nor %g
                     */
                    case (false, false) => generateNoSignNoG(fstring, t, fs_spec)

                    /* %g

                    if 10e-4_f64 <= fabs(value) && fabs(value) < 10_f64.powi(precision) {
                      format_to_type_f(modified_spec, value)
                    } else {
                      format_to_type_e(modified_spec, value)
                    }
                     */
                    case (false, true) => generateNoSignG(fstring, t, fs_spec)

                    /* sign padding (' ')

                    if value < 0 {
                      format!("{...}")
                    } else {
                      format(" {...}")
                    }
                     */
                    case (true, false) => generateSignPaddingNoG(fstring, t, fs_spec)

                    case (true, true) => generateSignPaddingG(fstring, t, fs_spec)
                  }

                  fn_format = ApplicationExpression(
                    fn_format.applicable,
                    ArraySeq(fn_format.args(0), args(1)),
                    fn_format.location
                  )
                  expid -> fn_format
                } else {
                  expid -> defn
                }
              }
              case _ => expid -> defn
            }
          }
          case _ => expid -> defn
        }
    }
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
  def modifyFormatStringSlifts(
    spec: Specification,
    formatStrings: mutable.HashMap[String, (String, Location)]
  ): Specification = {
    val definitions = spec.definitions.flatMap {
      case (id, defi) if formatStrings.contains(id.fullName) =>
        var inputStream: ExpressionArg = null
        var inputStreamType: Type = null

        defi match {
          case ApplicationExpression(applicable, args, _) =>
            inputStream = args(1)

            applicable match {
              case TypeApplicationExpression(_, args, _) =>
                inputStreamType = args(1)
              case _ =>
            }
          case _ =>
        }

        val fs_spec = parseFormatString(formatStrings(id.fullName)._1, formatStrings(id.fullName)._2)
        val fstring = produceRustFormatString(fs_spec, formatStrings(id.fullName)._1, formatStrings(id.fullName)._2)

        val fn_format = FunctionExpression(
          List(),
          List((Identifier("x"), StrictEvaluation, inputStreamType)),
          Map(),
          (fs_spec.padSign, fs_spec.formatType == 'g') match {

            /* The following four methods generate funky format string code because Rusts format! macro supports
             * neither sign padding nor the %g format specifier.
             */

            /* neither sign padding (' ') nor %g
             */
            case (false, false) => generateNoSignNoG(fstring, inputStreamType, fs_spec)

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
            case (true, false) => generateSignPaddingNoG(fstring, inputStreamType, fs_spec)

            case (true, true) => generateSignPaddingG(fstring, inputStreamType, fs_spec)
          }
        )
        val slift_format = ApplicationExpression(
          ExternExpression(
            "slift",
            FunctionType(
              List(), // Identifier
              List( // Parameter
                (StrictEvaluation, inputStreamType),
                (StrictEvaluation, fn_format.tpe)
              ),
              InstantiatedType("Events", List(InstantiatedType("String", List()))) // Return type
            )
          ),
          ArraySeq( // slift arguments: input stream and lambda that calls format!
            inputStream,
            ExpressionRef(Identifier(s"fmt_${id.fullName}"), fn_format.tpe)
          )
        )

        Seq(
          id -> slift_format.asInstanceOf[DefinitionExpression],
          Identifier(s"fmt_${id.fullName}") -> fn_format.asInstanceOf[DefinitionExpression]
        )
      case other => Seq(other)
    }

    Specification(spec.annotations, spec.in, definitions, spec.out, spec.maxIdentifier)
  }

  /**
   * Parses the specified TeSSLa format string.
   *
   * @param fs The format string to parse.
   * @param loc The source location of the format string.
   * @return The extracted format string specification.
   */
  def parseFormatString(fs: String, loc: Location): FormatStringSpecification = {
    val spec: FormatStringSpecification = new FormatStringSpecification()

    if (fs.length < 2) {
      throw Diagnostics.CommandNotSupportedError("Invalid format string: '" + fs + "'", loc)
    }
    if (fs.charAt(0) != '%') {
      throw Diagnostics.CommandNotSupportedError("Format string does not start with '%': '" + fs + "'", loc)
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
              throw Diagnostics.CommandNotSupportedError("Invalid format string: '" + fs + "'", loc)
            }
          case '+' =>
            if (!spec.plusSign) {
              spec.plusSign = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string: '" + fs + "'", loc)
            }
          case ' ' =>
            if (!spec.padSign) {
              spec.padSign = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string: '" + fs + "'", loc)
            }
          case '#' =>
            if (!spec.altForm) {
              spec.altForm = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string: '" + fs + "'", loc)
            }
          case '0' =>
            if (!spec.zeroPad) {
              spec.zeroPad = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string: '" + fs + "'", loc)
            }
          case ',' =>
            if (!spec.localeSeparators) {
              spec.localeSeparators = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string: '" + fs + "'", loc)
            }
          case '(' =>
            if (!spec.encloseNegatives) {
              spec.encloseNegatives = true
            } else {
              throw Diagnostics.CommandNotSupportedError("Invalid format string: '" + fs + "'", loc)
            }
          case _ => break()
        }

        i += 1
      }
    }

    try {
      var widthFound = false
      // Extract width, don't allow zeroes
      if (fs.charAt(i) != '0' && fs.charAt(i).isDigit) {
        val j = i

        while (fs.charAt(i).isDigit) {
          i += 1
        }

        spec.width = Integer.parseInt(fs.substring(j, i))
        widthFound = true
      }

      var precisionFound = false

      // Extract precision
      if (fs.charAt(i) == '.') {
        i += 1
        val j = i

        while (fs.charAt(i).isDigit) {
          i += 1
        }

        spec.precision = Integer.parseInt(fs.substring(j, i))
        precisionFound = true
      }

      // Extract the format specifier
      spec.formatType = fs.charAt(i)

      // Set default precision to 6, if floating point format specifier given
      if (!precisionFound && spec.isFloat) {
        spec.precision = 6
      }
      if (!widthFound) {
        spec.leftJustify = true
      }

      // Determine whether the output should be in upper case
      if (spec.formatType.isUpper) {
        spec.uppercase = true
        spec.formatType = spec.formatType.toLower
      }
    } catch {
      case e: Exception => throw Diagnostics.CommandNotSupportedError("Invalid format string: " + e.getMessage, loc)
    }

    if (i + 1 != fs.length) {
      throw Diagnostics.CommandNotSupportedError("Invalid format string: '" + fs + "'", loc)
    }

    spec
  }

  /**
   * Translates the specified format string into a format string
   * that can be fed into Rusts format!-macro.
   *
   * @param fs The format string to translate.
   * @param loc The source location of the format string.
   * @return A format string for Rusts format!-macro.
   */
  def produceRustFormatString(fs: FormatStringSpecification, formatString: String, loc: Location): String = {
    if (fs.formatType == 'a') {
      throw Diagnostics.CommandNotSupportedError(
        "Hexadecimal floating point literals aren't supported in Rust format strings: " + formatString,
        loc
      )
    }
    if (fs.encloseNegatives) {
      throw Diagnostics.CommandNotSupportedError(
        "Enclosing negative numbers is not supported in Rust format strings: " + formatString,
        loc
      )
    }
    if (fs.localeSeparators) {
      throw Diagnostics.CommandNotSupportedError(
        "Locale-specific grouping separators aren't supported in Rust format strings: " + formatString,
        loc
      )
    }
    if (fs.isOther && fs.zeroPad) {
      throw Diagnostics.CommandNotSupportedError("Strings can't be zero-padded: " + formatString, loc)
    }
    if (fs.precision > 0 && fs.isInteger) {
      throw Diagnostics.CommandNotSupportedError("Integer formats can't use a precision: " + formatString, loc)
    }
    if (fs.padSign && fs.plusSign) {
      throw Diagnostics.CommandNotSupportedError("Don't specify + and space: " + formatString, loc)
    }
    if (fs.altForm && !fs.isInteger) {
      throw Diagnostics.CommandNotSupportedError(
        "Alternate form can't be used with non-integer types: " + formatString,
        loc
      )
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
      str += fs.width
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

  /**
   * Generates a format! call for a format string that specifies neither sign padding nor %g.
   *
   * @param fstring The format string.
   * @param inputStreamType The type of the input stream.
   * @return The format! call.
   */
  private def generateNoSignNoG(fstring: String, inputStreamType: Type, fs_spec: FormatStringSpecification) = {
    val format = ApplicationExpression(
      ExternExpression(
        "[rust]format",
        FunctionType( // The lambda
          List(), // Identifier
          List( // Parameter
            (StrictEvaluation, InstantiatedType("String", List())),
            (StrictEvaluation, inputStreamType)
          ),
          InstantiatedType("String", List()) // Return type
        )
      ),
      ArraySeq(StringLiteralExpression(fstring), ExpressionRef(Identifier("x"), inputStreamType))
    )

    // Generate code for uppercase strings (%S)
    if (fs_spec.uppercase) {
      ApplicationExpression(
        ExternExpression(
          "String_toUpper",
          FunctionType(
            List(),
            List((StrictEvaluation, InstantiatedType("String", List()))),
            InstantiatedType("String", List())
          )
        ),
        ArraySeq(
          format
        )
      )
    } else {
      format
    }
  }

  /**
   * Generates a format! call for a format string that specifies sign padding but not %g.
   *
   * @param fstring The format string.
   * @param inputStreamType The type of the input stream.
   * @param fs_spec The format string specification.
   * @return The format! call.
   */
  private def generateSignPaddingNoG(fstring: String, inputStreamType: Type, fs_spec: FormatStringSpecification) = {
    ApplicationExpression(
      ExternExpression(
        "ite",
        FunctionType( // The if
          List(), // Identifier
          List( // Parameter
            (StrictEvaluation, InstantiatedType("Bool", List())),
            (LazyEvaluation, InstantiatedType("String", List())),
            (LazyEvaluation, InstantiatedType("String", List()))
          ),
          InstantiatedType("String", List()) // Return type
        )
      ),
      ArraySeq( // Arguments of the if: (exp : bool, then : T, else : T)
        (fs_spec.isInteger, fs_spec.isFloat) match {
          case (true, false) =>
            ApplicationExpression(
              ExternExpression(
                "lt", // value < 10_f64.powi(precision)
                FunctionType(
                  List(),
                  List(
                    (StrictEvaluation, InstantiatedType("Int", List())),
                    (StrictEvaluation, InstantiatedType("Int", List()))
                  ),
                  InstantiatedType("Bool", List())
                )
              ),
              ArraySeq( // Arguments of lt (<)
                ExpressionRef(Identifier("x"), inputStreamType),
                IntLiteralExpression(0)
              )
            )
          case (false, true) =>
            ApplicationExpression(
              ExternExpression(
                "flt", // value < 0.0
                FunctionType(
                  List(),
                  List(
                    (StrictEvaluation, InstantiatedType("Float", List())),
                    (StrictEvaluation, InstantiatedType("Float", List()))
                  ),
                  InstantiatedType("Bool", List())
                )
              ),
              ArraySeq( // Arguments of flt (<)
                ExpressionRef(Identifier("x"), inputStreamType),
                FloatLiteralExpression(0.0)
              )
            )
          case _ => ???
        },
        ApplicationExpression(
          ExternExpression(
            "[rust]format", // the value is negative, don't add padding
            FunctionType( // The lambda
              List(), // Identifier
              List( // Parameter
                (StrictEvaluation, InstantiatedType("String", List())),
                (StrictEvaluation, inputStreamType)
              ),
              InstantiatedType("String", List()) // Return type
            )
          ),
          ArraySeq(StringLiteralExpression(fstring), ExpressionRef(Identifier("x"), inputStreamType))
        ),
        ApplicationExpression(
          ExternExpression(
            "[rust]format", // the value is positive, add padding
            FunctionType( // The lambda
              List(), // Identifier
              List( // Parameter
                (StrictEvaluation, InstantiatedType("String", List())),
                (StrictEvaluation, inputStreamType)
              ),
              InstantiatedType("String", List()) // Return type
            )
          ),
          ArraySeq(
            StringLiteralExpression(if (fs_spec.leftJustify) { fstring.replace("{", " {") }
            else { fstring }),
            ExpressionRef(Identifier("x"), inputStreamType)
          )
        )
      )
    )
  }

  /**
   * Generates a format! call for a format string that specifies sign padding and %g.
   *
   * @param fstring The format string.
   * @param inputStreamType The type of the input stream.
   * @param fs_spec The format string specification.
   * @return The format! call.
   */
  private def generateSignPaddingG(fstring: String, inputStreamType: Type, fs_spec: FormatStringSpecification) = {
    ApplicationExpression(
      ExternExpression(
        "ite",
        FunctionType( // The if
          List(), // Identifier
          List( // Parameter
            (StrictEvaluation, InstantiatedType("Bool", List())),
            (LazyEvaluation, InstantiatedType("String", List())),
            (LazyEvaluation, InstantiatedType("String", List()))
          ),
          InstantiatedType("String", List()) // Return type
        )
      ),
      ArraySeq( // Arguments of the if: (exp : bool, then : T, else : T)
        ApplicationExpression(
          ExternExpression(
            "flt", // value < 0.0
            FunctionType(
              List(),
              List(
                (StrictEvaluation, InstantiatedType("Float", List())),
                (StrictEvaluation, InstantiatedType("Float", List()))
              ),
              InstantiatedType("Bool", List())
            )
          ),
          ArraySeq( // Arguments of flt (<)
            ExpressionRef(Identifier("x"), inputStreamType),
            FloatLiteralExpression(0.0)
          )
        ),
        generateNoSignG(fstring, inputStreamType, fs_spec),
        generateNoSignG(
          if (fs_spec.leftJustify) { fstring.replace("{", " {") }
          else { fstring },
          inputStreamType,
          fs_spec
        )
      )
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
  private def generateNoSignG(fstring: String, inputStreamType: Type, fs_spec: FormatStringSpecification) = {
    ApplicationExpression(
      ExternExpression(
        "ite",
        FunctionType( // The if
          List(), // Identifier
          List( // Parameter
            (StrictEvaluation, InstantiatedType("Bool", List())),
            (LazyEvaluation, InstantiatedType("String", List())),
            (LazyEvaluation, InstantiatedType("String", List()))
          ),
          InstantiatedType("String", List()) // Return type
        )
      ),
      ArraySeq( // Arguments of the if: (exp : bool, then : T, else : T)
        ApplicationExpression(
          ExternExpression(
            "and", // &&
            FunctionType(
              List(), // Identifier
              List( // Parameter
                (LazyEvaluation, InstantiatedType("Bool", List())),
                (LazyEvaluation, InstantiatedType("Bool", List()))
              ),
              InstantiatedType("Bool", List()) // Return type
            )
          ),
          ArraySeq( // Arguments of the &&: (op1 : bool, op2 : bool)
            ApplicationExpression(
              ExternExpression(
                "fleq", // 10e-4_f64 <= value
                FunctionType(
                  List(),
                  List(
                    (StrictEvaluation, InstantiatedType("Float", List())),
                    (StrictEvaluation, InstantiatedType("Float", List()))
                  ),
                  InstantiatedType("Bool", List())
                )
              ),
              ArraySeq( // Arguments of fleq (<=)
                FloatLiteralExpression(10e-4),
                ApplicationExpression(
                  ExternExpression(
                    "fabs", // abs(value)
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
                ) // end of abs(value)
              )
            ),
            ApplicationExpression(
              ExternExpression(
                "flt", // value < 10_f64.powi(precision)
                FunctionType(
                  List(),
                  List(
                    (StrictEvaluation, InstantiatedType("Float", List())),
                    (StrictEvaluation, InstantiatedType("Float", List()))
                  ),
                  InstantiatedType("Bool", List())
                )
              ),
              ArraySeq( // Arguments of flt (<)
                ApplicationExpression(
                  ExternExpression(
                    "fabs", // abs(value)
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
                ), // end of abs(value)
                FloatLiteralExpression(Math.pow(10, fs_spec.precision))
              )
            )
          ) // end of && arguments
        ),
        ApplicationExpression(
          ExternExpression(
            "[rust]format", // format_to_type_f
            FunctionType( // The lambda
              List(), // Identifier
              List( // Parameter
                (StrictEvaluation, InstantiatedType("String", List())),
                (StrictEvaluation, inputStreamType)
              ),
              InstantiatedType("String", List()) // Return type
            )
          ),
          ArraySeq(StringLiteralExpression(fstring), ExpressionRef(Identifier("x"), inputStreamType))
        ),
        ApplicationExpression(
          ExternExpression(
            "[rust]format", // format_to_type_e
            FunctionType( // The lambda
              List(), // Identifier
              List( // Parameter
                (StrictEvaluation, InstantiatedType("String", List())),
                (StrictEvaluation, inputStreamType)
              ),
              InstantiatedType("String", List()) // Return type
            )
          ),
          ArraySeq(
            StringLiteralExpression(
              fstring.replace(
                "}",
                if (fs_spec.uppercase) { "E}" }
                else { "e}" }
              )
            ),
            ExpressionRef(Identifier("x"), inputStreamType)
          )
        )
      )
    )
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
     * Determines whether this format specifies neither an integer nor a floating
     * point format.
     */
    def isOther: Boolean = {
      !isFloat && !isInteger
    }

    /**
     * Determines whether this format specifier specifies an integer format.
     *
     */
    def isInteger: Boolean = {
      formatType == 'x' || formatType == 'o' || formatType == 'd'
    }

    /**
     * Determines whether this format specifier specifies a floating point format.
     */
    def isFloat: Boolean = {
      formatType == 'f' || formatType == 'g' || formatType == 'e' || formatType == 'a'
    }
  }
}
