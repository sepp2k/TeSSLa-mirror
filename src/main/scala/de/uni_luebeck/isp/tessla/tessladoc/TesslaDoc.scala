package de.uni_luebeck.isp.tessla.tessladoc

import de.uni_luebeck.isp.tessla.Errors.InternalError
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla._
import org.antlr.v4.runtime.{CharStream, ParserRuleContext, Token}

import scala.collection.JavaConverters._

sealed abstract class TesslaDoc extends TesslaDoc.DocElement {
  def isGlobal: Boolean
}

object TesslaDoc {
  trait DocElement {
    def toJSON: String
    override def toString = toJSON
  }

  private def enquote(str: String) = "\"" + str + "\""

  private def jsonEscape(str: String) =
    str.replace("\n", "\\n").replace("\"", "\\\"")

  case class Docs(items: Seq[TesslaDoc]) extends DocElement {
    override def toJSON = items.mkString("[\n", ",\n", "\n]")

    def globalsOnly = Docs(items.filter(_.isGlobal))
  }

  case class TypeDoc(name: String, typeParameters: Seq[String], doc: String, loc: Location) extends TesslaDoc {
    override def toJSON = {
      s"""{
         |  "kind": "type",
         |  "location": ${loc.toJSON},
         |  "scope": "global",
         |  "name": "$name",
         |  "typeParameters": ${typeParameters.map(enquote).mkString("[", ", ", "]")},
         |  "doc": "${jsonEscape(doc)}"
         |}""".stripMargin
    }

    override def isGlobal = true
  }

  case class Param(name: String, typ: String) extends DocElement {
    override def toJSON = s"""{"name": "$name", "type": "$typ"}"""
  }

  sealed abstract class Scope extends DocElement

  case object Global extends Scope {
    override def toJSON = "\"global\""
  }

  case class Local(scopeLocation: Location) extends Scope {
    override def toJSON = scopeLocation.toJSON
  }

  case class DefDoc( annotations: Seq[String],
                     name: String,
                     typeParameters: Seq[String],
                     parameters: Seq[Param],
                     returnType: Option[String],
                     scope: Scope,
                     doc: String,
                     loc: Location) extends TesslaDoc {
    override def toJSON = {
      var str =
        s"""{
           |  "kind": "def",
           |  "location": ${loc.toJSON},
           |  "scope": $scope
           |  "name": "$name",
           |  "typeParameters": ${typeParameters.map(enquote).mkString("[", ", ", "]")},
           |  "parameters": ${parameters.mkString("[", ", ", "]")},
           |""".stripMargin
      returnType.foreach(typ => str += s"""  "returnType": "$typ",\n""")
      str + s"""  "doc": "${jsonEscape(doc)}"\n}"""
    }

    override def isGlobal = scope == Global
  }

  class Extractor(spec: Seq[TesslaParser.ParseResult]) extends TranslationPhase.Translator[Docs] {
    override protected def translateSpec() = {
      Docs(spec.flatMap(_.tree.statements.asScala.flatMap(translateStatement)))
    }

    def translateStatement(definition: TesslaSyntax.StatementContext): Seq[TesslaDoc] = {
      new StatementVisitor(Global)(definition)
    }

    class StatementVisitor(scope: Scope) extends TesslaSyntaxBaseVisitor[Seq[TesslaDoc]]
      with (ParserRuleContext => Seq[TesslaDoc]) {
      // If the visitor returns null (because we didn't define the visitor method), return an empty Seq instead
      override def apply(ctx: ParserRuleContext) = Option(visit(ctx)).getOrElse(Seq())

      override def visitDef(definition: TesslaSyntax.DefContext) = {
        val header = definition.header
        val doc = DefDoc(
          annotations = header.annotations.asScala.map(_.getText),
          name = header.name.getText,
          typeParameters = header.typeParameters.asScala.map(_.getText),
          parameters = header.parameters.asScala.map(p => Param(p.ID.getText, p.parameterType.getText)),
          returnType = Option(header.resultType).map(_.getText),
          scope = scope,
          doc = getDoc(header.tessladoc.asScala),
          loc = Location.fromNode(definition)
        )
        val body = definition.body
        body match {
          case body: TesslaSyntax.ExpressionBodyContext =>
            val whereLoc = Option(body.LBRACE).map(lb => Location.fromToken(lb).merge(Location.fromToken(body.RBRACE)))
            val whereDefs = whereLoc.map(loc => body.defs.asScala.flatMap(new StatementVisitor(Local(loc))))
            doc +: (this(body.expression) ++ whereDefs.getOrElse(Seq()))
          case _ =>
            Seq(doc)
        }
      }

      override def visitTypeDefinition(typeDef: TesslaSyntax.TypeDefinitionContext) = {
        Seq(TypeDoc(
          name = typeDef.name.getText,
          typeParameters = typeDef.typeParameters.asScala.map(_.getText),
          doc = getDoc(typeDef.tessladoc.asScala),
          loc = Location.fromNode(typeDef)
        ))
      }

      override def visitBlock(block: TesslaSyntax.BlockContext) = {
        val visitor = new StatementVisitor(Local(Location.fromNode(block)))
        visitor(block.expression) ++ block.definitions.asScala.flatMap(visitor)
      }
    }

    def getDoc(lines: Seq[Token]): String = {
      // Filter out the empty lines and only keep the ones with tessladoc comments
      val docLines = lines.filter(_.getType == TesslaLexer.DOCLINE)
      docLines.map(_.getText.replaceAll("^--- ?|^## ?", "")).mkString
    }
  }

  def extract(srcs: Seq[CharStream], includeResolver: Option[String => Option[CharStream]], includeStdlib: Boolean): Result[Docs] = {
    Result.runSequentially(srcs) { src =>
      val results =
        includeResolver.map(new TesslaParser.WithIncludes(_).translate(src)).getOrElse {
          TesslaParser.SingleFile.translate(src).map(Seq(_))
        }
      results.andThen(new Extractor(_).translate())
    }.andThen { docsForFiles =>
      if(includeStdlib) {
        forStdlib.map { docsForStdlib =>
          docsForStdlib +: docsForFiles
        }
      } else {
        Success(docsForFiles, Seq())
      }
    }.map(docs => Docs(docs.flatMap(_.items)))
  }

  def forStdlib: Result[Docs] = {
    IncludeResolvers.fromStdlibResource("stdlib.tessla").map { predef =>
      extract(Seq(predef), Some(IncludeResolvers.fromStdlibResource), includeStdlib = false)
    }.getOrElse {
      Failure(Seq(InternalError("Could not find standard library")), Seq())
    }
  }
}