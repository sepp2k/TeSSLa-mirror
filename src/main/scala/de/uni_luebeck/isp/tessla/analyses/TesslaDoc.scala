package de.uni_luebeck.isp.tessla.analyses

import de.uni_luebeck.isp.tessla._
import org.antlr.v4.runtime.{CharStream, ParserRuleContext, Token}

import scala.collection.JavaConverters._

object TesslaDoc {
  trait DocElement {
    def toJSON: String
    override def toString = toJSON
  }

  private def enquote(str: String) = "\"" + str + "\""

  case class TypeDoc(name: String, typeParameters: Seq[String], doc: String, loc: Location) extends TesslaDoc {
    override def toJSON = {
      s"""{
         |  "kind": "type",
         |  "location": ${loc.toJSON},
         |  "scope": "global",
         |  "name": "$name",
         |  "typeParameters": ${typeParameters.map(enquote).mkString("[", ", ", "]")},
         |  "doc": "$doc"
         |}""".stripMargin
    }
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
      str + s"""  "doc": "$doc"\n}"""
    }
  }

  class Extractor(src: CharStream, currentFileOnly: Boolean)
    extends AbstractTesslaParser[Seq[TesslaDoc], Seq[TesslaDoc]](src) {
    override def aggregateItems(items: Seq[Seq[TesslaDoc]]) = items.flatten

    def translateStatement(definition: TesslaSyntax.StatementContext): Seq[TesslaDoc] = {
      new StatementVisitor(Global)(definition)
    }

    override def translateInclude(include: TesslaSyntax.IncludeContext) = {
      if (currentFileOnly) Seq()
      else super.translateInclude(include)
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
        val whereLoc = Option(body.LBRACE).map(lb => Location.fromToken(lb).merge(Location.fromToken(body.RBRACE)))
        val whereDefs = whereLoc.map(loc => body.defs.asScala.flatMap(new StatementVisitor(Local(loc))))
        doc +: (this(body.expression) ++ whereDefs.getOrElse(Seq()))
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

    def jsonEscape(str: String) = str.replace("\n", "\\n").replace("\"", "\\\"")

    def getDoc(lines: Seq[Token]): String = {
      // Filter out the empty lines and only keep the ones with tessladoc comments
      val docLines = lines.filter(_.getType == TesslaLexer.DOCLINE)
      jsonEscape(docLines.map(_.getText.replaceAll("--- ?|## ?", "")).mkString)
    }
  }

  def extract(src: CharStream, currentFileOnly: Boolean) = {
    new Extractor(src, currentFileOnly = currentFileOnly).translate()
  }

  def extractAsJSON(src: CharStream, currentFileOnly: Boolean) = {
    extract(src, currentFileOnly = currentFileOnly).map(_.mkString("[\n", ",\n", "\n]"))
  }
}

sealed abstract class TesslaDoc extends TesslaDoc.DocElement