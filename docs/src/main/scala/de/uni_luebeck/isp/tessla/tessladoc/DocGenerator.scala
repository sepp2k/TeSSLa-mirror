/*
 * Copyright 2020 The TeSSLa Community
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

package de.uni_luebeck.isp.tessla.tessladoc

import de.uni_luebeck.isp.tessla.core.{
  Location,
  TesslaLexer,
  TesslaParser,
  TesslaSyntax,
  TesslaSyntaxBaseVisitor,
  TranslationPhase
}
import de.uni_luebeck.isp.tessla.tessladoc.TesslaDoc.{
  AnnotationDoc,
  DefDoc,
  Docs,
  EvalType,
  FunctionType,
  Import,
  ModuleDoc,
  ObjectType,
  Param,
  SimpleType,
  TupleType,
  Type,
  TypeApplication,
  TypeDoc
}

import de.uni_luebeck.isp.tessla.core.Errors.InternalError
import de.uni_luebeck.isp.tessla.tessladoc.DocJsonProtocol._
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla.core._
import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.tree.RuleNode
import org.antlr.v4.runtime.{CharStream, ParserRuleContext, Token}

import scala.jdk.CollectionConverters._

/**
 * Extracts the documentation from a parsed specification.
 */
object DocGenerator {

  def apply(
    sources: Seq[CharStream],
    options: Compiler.Options,
    includeStdlib: Boolean,
    withIncludes: Boolean
  ): Result[Docs] = {
    resolveIncludes(sources, Option.when(withIncludes)(options.includeResolver))
      .andThen { docsForFiles =>
        val stdlib = if (includeStdlib) {
          options
            .stdlibIncludeResolver(options.stdlibPath)
            .map(predef => resolveIncludes(Seq(predef), Some(options.stdlibIncludeResolver)))
            .getOrElse { Failure(Seq(InternalError("Could not find standard library")), Seq()) }
        } else {
          Success(Seq(), Seq())
        }
        stdlib.map(_ ++ docsForFiles)
      }
      .map(docs => Docs(docs.flatMap(_.items), docs.flatMap(_.imports)))
      .map(docs => (Docs.apply _).tupled(qualifyImports(docs.items, docs.imports, Map(), Nil)))
  }

  private def qualifyImports(
    items: Seq[TesslaDoc],
    imports: Seq[Import],
    outerEnv: Map[String, List[String]],
    path: List[String]
  ): (Seq[TesslaDoc], Seq[Import]) = {
    val modules = items.collect {
      case module: ModuleDoc => module
    }
    val env = outerEnv ++ modules.map(m => m.name -> (path :+ m.name)).toMap

    val newImports = imports.map(imprt => Import(env(imprt.path.head) ++ imprt.path.tail))
    val newItems = items.map {
      case module: ModuleDoc =>
        val (members, imps) = qualifyImports(module.members, module.imports, env, path :+ module.name)
        module.copy(members = members, imports = imps)
      case item => item
    }
    (newItems, newImports)
  }

  private def resolveIncludes(
    sources: Seq[CharStream],
    includeResolver: Option[String => Option[CharStream]]
  ): Result[Seq[Docs]] = {
    Result.runSequentially(sources) { src =>
      val results = includeResolver
        .map(new TesslaParser.WithIncludes(_).translate(src))
        .getOrElse(TesslaParser.SingleFile.translate(src).map(Seq(_)))
      results.andThen(new Extractor(_).translate())
    }
  }

  private class Extractor(spec: Seq[TesslaParser.ParseResult]) extends TranslationPhase.Translator[Docs] {
    override protected def translateSpec(): Docs = {
      val (docs, imports) = translate(spec.flatMap(_.tree.entries.asScala.map(_.statement)))
      Docs(docs, imports)
    }

    def translate(statements: Seq[TesslaSyntax.StatementContext]): (Seq[TesslaDoc], Seq[Import]) = {
      val (docs, imports) = statements.flatMap(StatementVisitor.visit).partitionMap {
        case doc: TesslaDoc => Left(doc)
        case imprt: Import  => Right(imprt)
      }
      (docs.filter(_.doc != "nodoc"), imports)
    }

    def translateEvalType(evTyp: TesslaSyntax.EvalTypeContext): EvalType = {
      val typ = TypeVisitor.visit(evTyp.typ)
      val evaluation = Option(evTyp.evaluation).map(_.getText).getOrElse("")
      EvalType(evaluation, typ)
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

      // $COVERAGE-OFF$
      final override def visitChildren(node: RuleNode): Nothing = {
        throw InternalError(
          s"Undefined visitor method for ${node.getClass}",
          Location.fromNode(node.asInstanceOf[ParserRuleContext])
        )
      }
      // $COVERAGE-ON$
    }

    object StatementVisitor extends TesslaSyntaxBaseVisitor[Seq[Statement]] {

      override def visitDef(definition: TesslaSyntax.DefContext): Seq[Statement] = {
        val header = definition.header

        // Fetch the original source code, skipping documentation
        val start: Int =
          if (header.tessladoc.isEmpty) definition.start.getStartIndex
          else header.tessladoc.asScala.last.getStopIndex + 1
        val end: Int = definition.stop.getStopIndex
        val interval: Interval = new Interval(start, end)
        val source = definition.start.getInputStream.getText(interval)
        val leadingWhitespaces = source.segmentLength(_ == ' ')
        val unindented = source
          .split("\r?\n")
          .map(s => {
            val n = s.segmentLength(_ == ' ')
            s.drop(if (leadingWhitespaces > n) n else leadingWhitespaces)
          })
          .mkString("\n")

        val doc = DefDoc(
          name = header.name.getText,
          src = unindented,
          typeParameters = header.typeParameters.asScala.map(_.getText).toSeq,
          parameters = header.parameters.asScala
            .map(p => Param(p.ID.getText, translateEvalType(p.parameterType)))
            .toSeq,
          returnType = Option(header.resultType).map(TypeVisitor.visit),
          doc = getDoc(header.tessladoc.asScala.toSeq),
          loc = Location.fromNode(definition),
          isLiftable = definition.header.liftable != null
        )
        val body = definition.body
        body match {
          case body: TesslaSyntax.ExpressionBodyContext =>
            doc +: visit(body.expression)
          case _ =>
            Seq(doc)
        }
      }

      override def visitAnnotationDefinition(
        annotationDef: TesslaSyntax.AnnotationDefinitionContext
      ): Seq[AnnotationDoc] = {
        val (inModule, doc) = getInModule(getDoc(annotationDef.tessladoc.asScala.toSeq))
        val isGlobal = annotationDef.AT() == null
        Seq(
          AnnotationDoc(
            name = annotationDef.ID().getText,
            parameters = annotationDef.parameters.asScala
              .map(p => Param(p.ID.getText, translateEvalType(p.parameterType)))
              .toSeq,
            global = isGlobal,
            doc = doc,
            inModule = inModule,
            loc = Location.fromNode(annotationDef)
          )
        )
      }

      override def visitTypeDefinition(typeDef: TesslaSyntax.TypeDefinitionContext): Seq[TypeDoc] = {
        val (inModule, doc) = getInModule(getDoc(typeDef.tessladoc.asScala.toSeq))
        Seq(
          TypeDoc(
            name = typeDef.name.getText,
            typeParameters = typeDef.typeParameters.asScala.map(_.getText).toSeq,
            doc = doc,
            inModule = inModule,
            loc = Location.fromNode(typeDef)
          )
        )
      }

      override def visitBlock(block: TesslaSyntax.BlockContext): Seq[Statement] = Seq()

      override def visitModuleDefinition(module: TesslaSyntax.ModuleDefinitionContext): Seq[ModuleDoc] = {
        val (members, imports) = translate(module.contents.asScala.map(_.statement).toSeq)
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

    private def getDoc(lines: Seq[Token]): String = {
      // Filter out the empty lines and only keep the ones with tessladoc comments
      val docLines = lines.filter(_.getType == TesslaLexer.DOCLINE)
      docLines.map(_.getText.replaceAll("^--- ?|^## ?", "")).mkString.trim
    }

    private def getInModule(doc: String): (Option[String], String) = {
      val InModule = """^inmodule\s+(.+)\r?\n?(?s:(.*))$""".r
      doc match {
        case InModule(name, rest) => (Some(name), rest)
        case s                    => (None, s)
      }
    }
  }

}
