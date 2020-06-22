package de.uni_luebeck.isp.tessla.tessladoc

import de.uni_luebeck.isp.tessla.Errors.InternalError
import de.uni_luebeck.isp.tessla.tessladoc.DocJsonProtocol._
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla._
import org.antlr.v4.runtime.tree.RuleNode
import org.antlr.v4.runtime.{CharStream, ParserRuleContext, Token}

import scala.jdk.CollectionConverters._
import spray.json._

sealed trait Statement

sealed trait TesslaDoc extends Statement {
  def scope: TesslaDoc.Scope

  def doc: String

  final def isGlobal: Boolean = scope == TesslaDoc.Global

  def globalsOnly: Option[TesslaDoc] = if (isGlobal) Some(this) else None
}

sealed trait GlobalDoc extends TesslaDoc {
  final override def scope: TesslaDoc.Scope = TesslaDoc.Global
}

object TesslaDoc {

  case class Docs(items: Seq[TesslaDoc], imports: Seq[Import]) {
    def globalsOnly: Docs = Docs(items.flatMap(_.globalsOnly), imports)

    override def toString: String = this.toJson.prettyPrint
  }

  case class DefDoc(
    name: String,
    typeParameters: Seq[String],
    parameters: Seq[Param],
    returnType: Option[Type],
    scope: Scope,
    doc: String,
    loc: Location,
    isLiftable: Boolean
  ) extends TesslaDoc

  case class AnnotationDoc(name: String, parameters: Seq[Param], doc: String, loc: Location) extends GlobalDoc

  case class TypeDoc(name: String, typeParameters: Seq[String], doc: String, loc: Location) extends GlobalDoc

  case class ModuleDoc(name: String, doc: String, members: Seq[TesslaDoc], imports: Seq[Import], loc: Location)
      extends GlobalDoc {
    override def globalsOnly: Option[TesslaDoc] =
      if (isGlobal) Some(copy(members = members.flatMap(_.globalsOnly))) else None
  }

  case class Import(path: Seq[String]) extends Statement

  case class Param(name: String, typ: EvalType)

  sealed abstract class Type

  case class EvalType(eval: String, typ: Type)

  case class SimpleType(name: String) extends Type

  case class TypeApplication(constructor: Type, arguments: Seq[Type]) extends Type

  case class FunctionType(parameters: Seq[EvalType], result: Type) extends Type

  case class ObjectType(members: Map[String, Type]) extends Type

  case class TupleType(members: Seq[Type]) extends Type

  sealed abstract class Scope

  case object Global extends Scope

  case class Local(scopeLocation: Location) extends Scope

  class Extractor(spec: Seq[TesslaParser.ParseResult]) extends TranslationPhase.Translator[Docs] {
    override protected def translateSpec(): Docs = {
      val (docs, imports) = split(spec.flatMap(_.tree.entries.asScala.map(_.statement).flatMap(translateStatement)))
      Docs(docs.filter(_.doc != "nodoc"), imports)
    }

    def translateStatement(definition: TesslaSyntax.StatementContext): Seq[Statement] = {
      new StatementVisitor(Global).visit(definition)
    }

    object TypeVisitor extends TesslaSyntaxBaseVisitor[Type] {
      override def visitSimpleType(simpleType: TesslaSyntax.SimpleTypeContext): SimpleType =
        SimpleType(simpleType.name.getText)

      override def visitTypeApplication(typeApplication: TesslaSyntax.TypeApplicationContext): TypeApplication =
        TypeApplication(
          constructor = SimpleType(typeApplication.name.getText),
          arguments = typeApplication.typeArguments.asScala.map(visit).toSeq
        )

      override def visitFunctionType(functionType: TesslaSyntax.FunctionTypeContext): FunctionType =
        FunctionType(
          parameters = functionType.parameterTypes.asScala.map(translateEvalType).toSeq,
          result = visit(functionType.resultType)
        )

      override def visitTupleType(tupleType: TesslaSyntax.TupleTypeContext): TupleType =
        TupleType(members = tupleType.elementTypes.asScala.map(visit).toSeq)

      override def visitObjectType(objectType: TesslaSyntax.ObjectTypeContext): ObjectType =
        ObjectType(members = objectType.memberSigs.asScala.map { memberSig =>
          memberSig.name.getText -> visit(memberSig.`type`())
        }.toMap)

      final override def visitChildren(node: RuleNode): Nothing = {
        throw InternalError(
          s"Undefined visitor method for ${node.getClass}",
          Location.fromNode(node.asInstanceOf[ParserRuleContext])
        )
      }
    }

    def translateEvalType(evTyp: TesslaSyntax.EvalTypeContext): EvalType = {
      val typ = TypeVisitor.visit(evTyp.typ)
      val evaluation = Option(evTyp.evaluation).map(_.getText).getOrElse("")
      EvalType(evaluation, typ)
    }

    class StatementVisitor(scope: Scope) extends TesslaSyntaxBaseVisitor[Seq[Statement]] {

      override def visitDef(definition: TesslaSyntax.DefContext): Seq[Statement] = {
        val header = definition.header
        val doc = DefDoc(
          name = header.name.getText,
          typeParameters = header.typeParameters.asScala.map(_.getText).toSeq,
          parameters = header.parameters.asScala
            .map(p => Param(p.ID.getText, translateEvalType(p.parameterType)))
            .toSeq,
          returnType = Option(header.resultType).map(TypeVisitor.visit),
          scope = scope,
          doc = getDoc(header.tessladoc.asScala.toSeq),
          loc = Location.fromNode(definition),
          isLiftable = definition.header.liftable != null
        )
        val body = definition.body
        body match {
          case body: TesslaSyntax.ExpressionBodyContext =>
            val whereLoc = Option(body.LBRACE).map(lb => Location.fromToken(lb).merge(Location.fromToken(body.RBRACE)))
            val whereDefs =
              whereLoc.map(loc => body.defs.asScala.flatMap(new StatementVisitor(Local(loc)).visit))
            doc +: (visit(body.expression) ++ whereDefs.getOrElse(Seq()))
          case _ =>
            Seq(doc)
        }
      }

      override def visitAnnotationDefinition(
        annotationDef: TesslaSyntax.AnnotationDefinitionContext
      ): Seq[AnnotationDoc] = {
        Seq(
          AnnotationDoc(
            name = annotationDef.ID().getText,
            parameters = annotationDef.parameters.asScala
              .map(p => Param(p.ID.getText, translateEvalType(p.parameterType)))
              .toSeq,
            doc = getDoc(annotationDef.tessladoc.asScala.toSeq),
            loc = Location.fromNode(annotationDef)
          )
        )
      }

      override def visitTypeDefinition(typeDef: TesslaSyntax.TypeDefinitionContext): Seq[TypeDoc] = {
        Seq(
          TypeDoc(
            name = typeDef.name.getText,
            typeParameters = typeDef.typeParameters.asScala.map(_.getText).toSeq,
            doc = getDoc(typeDef.tessladoc.asScala.toSeq),
            loc = Location.fromNode(typeDef)
          )
        )
      }

      override def visitBlock(block: TesslaSyntax.BlockContext): Seq[Statement] = {
        val visitor = new StatementVisitor(Local(Location.fromNode(block))).visit _
        visitor(block.expression) ++ block.definitions.asScala.flatMap(visitor)
      }

      override def visitModuleDefinition(module: TesslaSyntax.ModuleDefinitionContext): Seq[ModuleDoc] = {
        val (members, imports) = split(module.contents.asScala.map(_.statement).flatMap(visit).toSeq)
        Seq(
          ModuleDoc(
            module.name.getText,
            getDoc(module.tessladoc.asScala.toSeq),
            members,
            imports,
            Location.fromNode(module)
          )
        )
      }

      override def visitImportStatement(ctx: TesslaSyntax.ImportStatementContext): Seq[Import] =
        Seq(Import(ctx.path.asScala.map(_.getText).toList))

      override def defaultResult(): Seq[TesslaDoc] = Seq()
    }

    private def split(s: Seq[Statement]): (Seq[TesslaDoc], Seq[Import]) = s.partitionMap {
      case doc: TesslaDoc => Left(doc)
      case imprt: Import  => Right(imprt)
    }

    private def getDoc(lines: Seq[Token]): String = {
      // Filter out the empty lines and only keep the ones with tessladoc comments
      val docLines = lines.filter(_.getType == TesslaLexer.DOCLINE)
      docLines.map(_.getText.replaceAll("^--- ?|^## ?", "")).mkString.trim
    }
  }

  def extract(
    sources: Seq[CharStream],
    includeResolver: Option[String => Option[CharStream]],
    includeStdlib: Boolean
  ): Result[Docs] = {
    Result
      .runSequentially(sources) { src =>
        val results =
          includeResolver.map(new TesslaParser.WithIncludes(_).translate(src)).getOrElse {
            TesslaParser.SingleFile.translate(src).map(Seq(_))
          }
        results.andThen(new Extractor(_).translate())
      }
      .andThen { docsForFiles =>
        if (includeStdlib) {
          forStdlib.map(_ +: docsForFiles)
        } else {
          Success(docsForFiles, Seq())
        }
      }
      .map(docs => Docs(docs.flatMap(_.items), docs.flatMap(_.imports)))
  }

  private def forStdlib: Result[Docs] = {
    IncludeResolvers
      .fromStdlibResource("stdlib.tessla")
      .map { predef =>
        extract(Seq(predef), Some(IncludeResolvers.fromStdlibResource), includeStdlib = false)
      }
      .getOrElse { Failure(Seq(InternalError("Could not find standard library")), Seq()) }
  }
}
