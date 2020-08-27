package de.uni_luebeck.isp.tessla.core

import cats._
import cats.data.Ior
import cats.implicits._
import de.uni_luebeck.isp.tessla.core.Errors.{InfiniteRecursion, InternalError, WithStackTrace}
import de.uni_luebeck.isp.tessla.core.TesslaAST.{Core, Typed}
import de.uni_luebeck.isp.tessla.core.util.LazyWithStack
import de.uni_luebeck.isp.tessla.core.util._

import scala.collection.mutable
import scala.collection.immutable.ArraySeq

object ConstantEvaluator {

  val PREFER_REIFIED_EXPRESSIONS = false

  type Env = Map[Typed.Identifier, TranslationResult[Any, Some]]
  type TypeEnv = Map[Typed.Identifier, Typed.Type]

  class TranslatedExpressions {
    val expressions = mutable.Map[Core.Identifier, Core.Expression]()
    val deferredQueue = mutable.Queue[() => Unit]()

    def complete(): Unit = {
      while (deferredQueue.nonEmpty) {
        deferredQueue.dequeue()()
      }
    }
  }

  case class Record(entries: Map[String, Any]) {
    override def toString = TesslaAST.printRecord(entries, " = ", (x: Any) => x.toString, "()")
  }

  case class RuntimeError(msg: String) // TODO: support location information etc

  val lazyWithStack = new LazyWithStack[Location]() {
    override def call[A](stack: Stack, rec: Boolean)(a: Stack => A) = {
      if (stack.size > 1000) {
        throw WithStackTrace(Errors.StackOverflow(stack.head), stack.tail)
      } else if (rec) {
        throw WithStackTrace(InfiniteRecursion(stack.head), stack.tail)
      } else {
        try {
          a(stack)
        } catch {
          case err: InternalError =>
            throw WithStackTrace(err, stack)
          case _: StackOverflowError =>
            throw WithStackTrace(Errors.StackOverflow(stack.head), stack.tail)
        }
      }
    }
  }

  import lazyWithStack._

  case class StrictOrLazy(
    translateStrict: StackLazy[Core.ExpressionArg],
    translateLazy: StackLazy[Core.ExpressionArg]
  )

  // Expression that can be either an expression (left) or a reference to an expression or an literal (right).
  // A reference (or a literal) can be requested with strict or lazy translation.
  // When lazy translation is requested, the translation of a a referenced expression will be deferred.
  type ExpressionOrRef = Either[StackLazy[Core.Expression], StrictOrLazy]

  case class TranslationResult[+A, +B[+_]](
    value: StackLazy[Option[A]],
    expression: StackLazy[B[ExpressionOrRef]]
  )

  case class Translatable[+A, +B[+_]](translate: TranslatedExpressions => TranslationResult[A, B])

  type TranslatableMonad[+A] = Translatable[A, Option]

  def getExpressionArgStrict(result: TranslationResult[Any, Some]) =
    result.expression.flatMap(_.value match {
      case Left(expression)      => expression
      case Right(expresssionRef) => expresssionRef.translateStrict
    })

  def mkLiteralResult(value: Option[Any], expression: Core.Expression) =
    TranslationResult[Any, Some](
      Lazy(value),
      Lazy(Some(Right(StrictOrLazy(Lazy(expression), Lazy(expression)))))
    )

  def reified(
    result: TranslationResult[Any, Option],
    tpe: Core.Type
  ): TranslationResult[Any, Some] =
    TranslationResult[Any, Some](
      result.value,
      getReified(result, tpe).map {
        case Right(e)    => Some(e)
        case Left(error) => throw error
      }
    )

  def getReified(
    result: TranslationResult[Any, Option],
    tpe: Core.Type
  ): StackLazy[Either[InternalError, ExpressionOrRef]] = {
    lazy val reified = result.value
      .flatMap { value =>
        value
          .map(Right(_))
          .getOrElse(Left(InternalError("No value available for reification.")))
          .traverse { v =>
            for {
              reified <- CompiletimeExterns.reify(v, tpe)
            } yield for {
              r <- reified
            } yield
              if (
                r.isInstanceOf[Core.IntLiteralExpression] || r
                  .isInstanceOf[Core.FloatLiteralExpression] ||
                r.isInstanceOf[Core.StringLiteralExpression] || r
                  .isInstanceOf[Core.ExternExpression]
              ) {
                Right(StrictOrLazy(Lazy(r), Lazy(r)))
              } else {
                Left(Lazy(r))
              }
          }
      }
      .map(_.flatten)
    for {
      r <- reified
      e <- result.expression
      either = e.map(Right(_)).getOrElse(Left(InternalError("No value available for reification.")))
    } yield
      if (PREFER_REIFIED_EXPRESSIONS) {
        r.orElse(either)
      } else {
        either.orElse(r)
      }
  }

  def tryReified(
    result: TranslationResult[Any, Option],
    tpe: Core.Type
  ): TranslationResult[Any, Option] =
    TranslationResult[Any, Option](result.value, getReified(result, tpe).map(_.toOption))

  type TypeApplicable = List[Typed.Type] => Applicable
  type Applicable = ArraySeq[Translatable[Any, Option]] => Translatable[Any, Some]

  type TypeExtern = List[Typed.Type] => Extern
  type Extern = ArraySeq[TranslatableMonad[Any]] => TranslatableMonad[Any]

  // TODO: shoud be declared in standard library
  val errorExternA = Core.Identifier(Ior.Left("A"))
  val errorExtern = Core.ExternExpression(
    "error",
    Core.FunctionType(
      List(errorExternA),
      List((TesslaAST.LazyEvaluation, Core.StringType)),
      Core.TypeParam(errorExternA)
    )
  )

  def mkErrorResult(error: RuntimeError, tpe: Core.Type) = {
    val expression = Core.ApplicationExpression(
      Core.TypeApplicationExpression(errorExtern, List(tpe)),
      ArraySeq(Core.StringLiteralExpression(error.msg))
    )
    TranslationResult[Any, Some](Lazy(Some(error)), Lazy(Some(Left(Lazy(expression)))))
  }

  val noExtern: TypeExtern = _ =>
    _ => Translatable[Any, Option](_ => TranslationResult[Any, Option](Lazy(None), Lazy(None)))

  val doNotExpandExterns: Map[String, TypeExtern] =
    List("CTF_getInt", "CTF_getString").map(_ -> noExtern).toMap

  implicit val translatableMonad: Monad[TranslatableMonad] = new Monad[TranslatableMonad] {
    override def flatMap[A, B](fa: TranslatableMonad[A])(f: A => TranslatableMonad[B]) =
      Translatable { translatedExpressions =>
        val result = fa
          .translate(translatedExpressions)
          .value
          .map(_.map(a => f(a).translate(translatedExpressions)))
        val value = result.flatMap(_.traverse(_.value)).map(_.flatten)
        val expression = result.flatMap(_.traverse(_.expression)).map(_.flatten)
        TranslationResult(value, expression)
      }

    override def tailRecM[A, B](
      a: A
    )(f: A => TranslatableMonad[Either[A, B]]): TranslatableMonad[B] =
      flatMap(f(a)) {
        case Right(b)    => pure(b)
        case Left(nextA) => tailRecM(nextA)(f)
      }

    override def pure[A](x: A): TranslatableMonad[A] =
      Translatable(_ => TranslationResult[A, Option](Lazy(Some(x)), Lazy(None)))
  }

  val valueExterns: Map[String, Any] = RuntimeExterns
    .commonExterns[TranslatableMonad]
    .view
    .mapValues {
      case f: Extern => (_: List[Typed.Type]) => f
      case v         => v
    }
    .toMap

  val externs = valueExterns.withDefaultValue(noExtern)

}

class ConstantEvaluator extends TranslationPhase[Typed.Specification, Core.Specification] {
  override def translate(spec: Typed.Specification) = {
    new ConstantEvaluatorWorker(spec).translate()
  }
}

class ConstantEvaluatorWorker(spec: Typed.Specification) extends TranslationPhase.Translator[Core.Specification] {

  import ConstantEvaluator._
  import lazyWithStack._

  override def translateSpec(): Core.Specification = try {
    val translatedExpressions = new TranslatedExpressions

    val inputs: Env = spec.in.map { i =>
      val ref = Lazy(Core.ExpressionRef(translateIdentifier(i._1), translateType(i._2._1, Map())))
      i._1 -> TranslationResult[Any, Some](Lazy(None), Lazy(Some(Right(StrictOrLazy(ref, ref)))))
    }

    lazy val env: Env = inputs ++ spec.definitions.map { entry =>
      val result = translateExpressionArg(entry._2, env, Map(), entry._1.idOrName.left)
        .translate(translatedExpressions)
      lazy val id = translateIdentifier(entry._1)
      (
        entry._1,
        pushStack(
          TranslationResult[Any, Some](
            result.value,
            StackLazy { stack =>
              Some(result.expression.get(stack).value match {
                case Left(expression) =>
                  Right(
                    StrictOrLazy(
                      StackLazy { stack =>
                        translatedExpressions.expressions += id -> expression.get(stack)
                        Core.ExpressionRef(id, translateType(entry._2.tpe, Map()))
                      },
                      StackLazy { stack =>
                        translatedExpressions.deferredQueue += (() =>
                          translatedExpressions.expressions += id -> expression.get(stack)
                        )
                        Core.ExpressionRef(id, translateType(entry._2.tpe, Map()))
                      }
                    )
                  )
                case Right(expressionRef) => Right(expressionRef)
              })
            }
          ),
          entry._1.location
        )
      )
    }

    val outputs = spec.out.map { x =>
      val annotations = x._2.view
        .mapValues(
          _.map(a =>
            getExpressionArgStrict(
              translateExpressionArg(a, env, Map(), None).translate(translatedExpressions)
            ).get(Nil)
          )
        )
        .toMap
      (
        env(x._1.id).expression.get(List(x._1.location)).get match {
          case Left(expression) =>
            val id = translateIdentifier(x._1.id)
            translatedExpressions.expressions += id -> expression.get(List(x._1.location))
            Core.ExpressionRef(id, expression.get(List(x._1.location)).tpe, x._1.location)
          case Right(ref) =>
            ref.translateStrict.get(List(x._1.location)) match {
              case Core.ExpressionRef(id, tpe, _) =>
                Core.ExpressionRef(id, tpe, x._1.location)
              case expression: Core.Expression =>
                val id = translateIdentifier(x._1.id)
                translatedExpressions.expressions += id -> expression
                Core.ExpressionRef(id, expression.tpe, x._1.location)
            }
        },
        annotations
      )
    }

    val ins = spec.in.map { x =>
      val annotations = x._2._2.view
        .mapValues(
          _.map(a =>
            getExpressionArgStrict(
              translateExpressionArg(a, env, Map(), None).translate(translatedExpressions)
            ).get(Nil)
          )
        )
        .toMap
      (translateIdentifier(x._1), (translateType(x._2._1, Map()), annotations))
    }

    translatedExpressions.complete()
    Core.Specification(ins, translatedExpressions.expressions.toMap, outputs, counter)
  } catch {
    case e: ClassCastException =>
      throw InternalError(e.toString + "\n" + e.getStackTrace.mkString("\n"))
  }

  def translateExpressionArg(
    expr: Typed.ExpressionArg,
    env: => Env,
    typeEnv: TypeEnv,
    nameOpt: Option[String]
  ): Translatable[Any, Some] = Translatable { translatedExpressions =>
    expr match {
      case Typed.ExpressionRef(id, tpe, location) =>
        lazy val result = env(id)
        lazy val newId = makeIdentifier(nameOpt, id.location)
        pushStack(
          TranslationResult[Any, Some](
            StackLazy { stack =>
              result.value.get(stack)
            },
            StackLazy { stack =>
              Some((result.expression.get(stack).value match {
                case Left(expression) =>
                  Right(
                    StrictOrLazy(
                      StackLazy { stack =>
                        translatedExpressions.expressions += newId -> expression.get(stack)
                        Core.ExpressionRef(newId, translateType(tpe, typeEnv))
                      },
                      StackLazy { stack =>
                        translatedExpressions.deferredQueue += (() =>
                          translatedExpressions.expressions += newId -> expression.get(stack)
                        )
                        Core.ExpressionRef(newId, translateType(tpe, typeEnv))
                      }
                    )
                  )
                case Right(id) => Right(id)
              }): ExpressionOrRef)
            }
          ),
          location
        )
      case Typed.IntLiteralExpression(value, location) =>
        mkLiteralResult(Some(value), Core.IntLiteralExpression(value, location))
      case Typed.FloatLiteralExpression(value, location) =>
        mkLiteralResult(Some(value), Core.FloatLiteralExpression(value, location))
      case Typed.StringLiteralExpression(value, location) =>
        mkLiteralResult(Some(value), Core.StringLiteralExpression(value, location))
      case Typed.ExternExpression(name, tpe, location) =>
        val expression = Core.ExternExpression(
          name,
          translateType(tpe, typeEnv),
          location
        )
        val value = tpe match {
          case Typed.FunctionType(typeParams, params, resultType, _) =>
            Lazy {
              val f: TypeApplicable = typeArgs => {
                val innerTypeEnv = typeEnv ++ typeParams.zip(typeArgs)
                args =>
                  Translatable { translatedExpressions =>
                    val extern = externs(name).asInstanceOf[TypeExtern]
                    val result = tryReified(
                      extern(typeArgs)(args).translate(translatedExpressions),
                      translateType(resultType, innerTypeEnv)
                    )
                    TranslationResult[Any, Some](
                      result.value,
                      result.expression.map(e =>
                        Some(e.getOrElse(Left(StackLazy { stack =>
                          mkApplicationExpression(
                            Core.TypeApplicationExpression(
                              expression,
                              typeArgs.map(translateType(_, innerTypeEnv))
                            ),
                            params,
                            args
                              .zip(params)
                              .map(a =>
                                reified(
                                  a._1.translate(translatedExpressions),
                                  translateType(a._2._2, innerTypeEnv)
                                )
                              ),
                            innerTypeEnv,
                            stack,
                            translatedExpressions,
                            location
                          )
                        })))
                      )
                    )
                  }
              }
              Some(if (typeParams.isEmpty) f(Nil) else f)
            }: StackLazy[Option[Any]]
          case _ => Lazy(Some(externs(name)))
        }
        TranslationResult[Any, Some](
          value,
          Lazy(Some(Right(StrictOrLazy(Lazy(expression), Lazy(expression)))))
        )
      case Typed.FunctionExpression(typeParams, params, body, result, location) =>
        val ref = StackLazy { stack =>
          val id = makeIdentifier(nameOpt)
          translatedExpressions.deferredQueue += (() => {
            val innerTranslatedExpressions = new TranslatedExpressions
            val innerTypeEnv = typeEnv -- typeParams

            val translatedParams = params.map { param =>
              val paramId = translateIdentifier(param._1)
              val tpe = translateType(param._3, innerTypeEnv)
              val ref = Lazy(Core.ExpressionRef(paramId, tpe))
              param._1 -> (paramId, translateEvaluation(param._2), tpe,
              TranslationResult[Any, Some](Lazy(None), Lazy(Some(Right(StrictOrLazy(ref, ref))))))
            }

            lazy val innerEnv: Env =
              env ++ translatedParams.map(x => (x._1, x._2._4)) ++ translatedBody
            lazy val translatedBody = body.map { entry =>
              entry._1 -> translateExpressionArg(
                entry._2,
                innerEnv,
                innerTypeEnv,
                entry._1.idOrName.left
              ).translate(innerTranslatedExpressions)
            }

            val translatedResult = getExpressionArgStrict(
              pushStack(
                translateExpressionArg(result, innerEnv, innerTypeEnv, None)
                  .translate(innerTranslatedExpressions),
                location
              )
            ).get(stack)

            innerTranslatedExpressions.complete()

            val f = Core.FunctionExpression(
              typeParams.map(translateIdentifier),
              translatedParams.map(x => (x._2._1, x._2._2, x._2._3)),
              innerTranslatedExpressions.expressions.toMap,
              translatedResult,
              location
            )
            translatedExpressions.expressions += id -> f
          })

          Core.ExpressionRef(id, translateType(expr.tpe, typeEnv))
        }
        val value: StackLazy[Option[Any]] = Lazy {
          val f: TypeApplicable = typeArgs => {
            args =>
              Translatable { translatedExpressions =>
                val innerTypeEnv = typeEnv ++ typeParams.zip(typeArgs)
                lazy val innerEnv: Env = env ++ params
                  .zip(args)
                  .map(a =>
                    (a._1._1 -> reified(
                      a._2.translate(translatedExpressions),
                      translateType(a._1._3, innerTypeEnv)
                    ))
                  ) ++
                  body.map(e =>
                    (
                      e._1,
                      translateExpressionArg(e._2, innerEnv, innerTypeEnv, e._1.idOrName.left)
                        .translate(translatedExpressions)
                    )
                  )
                translateExpressionArg(result, innerEnv, innerTypeEnv, nameOpt).translate(
                  translatedExpressions
                )
              }
          }
          Some(if (typeParams.isEmpty) f(Nil) else f)
        }
        pushStack(
          TranslationResult[Any, Some](value, Lazy(Some(Right(StrictOrLazy(ref, ref))))),
          location
        )
      case Typed.RecordConstructorExpression(entries, location) =>
        val translatedEntries = mapValues(entries)(x =>
          (
            translateExpressionArg(x._1, env, typeEnv, nameOpt).translate(translatedExpressions),
            x._2
          )
        )
        val value = Lazy(Some(Record(translatedEntries.map(x => (x._1, x._2._1)))))
        val expression = StackLazy { _ =>
          Some(Left(StackLazy { stack =>
            Core.RecordConstructorExpression(
              translatedEntries
                .map(x => (x._1, (getExpressionArgStrict(x._2._1).get(stack), x._2._2))),
              location
            )
          }))
        }
        pushStack(TranslationResult[Any, Some](value, expression), location)
      case Typed.RecordAccessorExpression(name, entries, nameLocation, location) =>
        lazy val translatedEntries =
          translateExpressionArg(entries, env, typeEnv, nameOpt).translate(translatedExpressions)
        val Typed.RecordType(entryTypes, _) = entries.tpe
        val value = StackLazy { stack =>
          translatedEntries.value.get(stack).map {
            case record: Record =>
              record.entries(name).asInstanceOf[TranslationResult[Any, Some]]
            case error: RuntimeError =>
              mkErrorResult(error, translateType(entryTypes(name)._1, typeEnv))
          }
        }
        val expression = StackLazy { stack =>
          value
            .get(stack)
            .map(_.expression.get(stack))
            .getOrElse(Some(Left(StackLazy { stack =>
              Core.RecordAccessorExpression(
                name,
                getExpressionArgStrict(translatedEntries).get(stack),
                nameLocation,
                location
              )
            })))
        }
        pushStack(
          TranslationResult[Any, Some](
            value.flatMap(_.traverse(_.value).map(_.flatten)),
            expression
          ),
          location
        )
      case Typed.ApplicationExpression(applicable, args, location) =>
        lazy val translatedApplicable =
          translateExpressionArg(applicable, env, typeEnv, nameOpt).translate(translatedExpressions)
        val Typed.FunctionType(_, paramTypes, resultType, _) = applicable.tpe
        if (paramTypes.size != args.size) {
          throw InternalError(s"Wrong number of arguments.", location)
        }
        lazy val translatedArgs =
          args.map(x => translateExpressionArg(x, env, typeEnv, nameOpt).translate(translatedExpressions))

        val value = StackLazy { stack =>
          if (
            translatedArgs.zip(paramTypes).forall { x =>
              x._2._1 != TesslaAST.StrictEvaluation || x._1.value.get(stack).nonEmpty
            }
          ) {
            // TODO: handle strict arguments differently here?
            translatedApplicable.value.get(stack).map {
              case error: RuntimeError =>
                mkErrorResult(error, translateType(resultType, typeEnv))
              case f =>
                f.asInstanceOf[Applicable](
                    translatedArgs.map(r => Translatable[Any, Some](_ => r))
                  )
                  .translate(translatedExpressions)
            }
          } else {
            None
          }
        }
        val expression = StackLazy { stack =>
          value
            .get(stack)
            .map(_.expression.get(stack))
            .getOrElse(Some(Left(StackLazy { stack =>
              mkApplicationExpression(
                getExpressionArgStrict(translatedApplicable).get(stack),
                paramTypes,
                translatedArgs,
                typeEnv,
                stack,
                translatedExpressions,
                location
              )
            })))
        }
        pushStack(
          TranslationResult[Any, Some](
            value.flatMap(_.traverse(_.value).map(_.flatten)),
            expression
          ),
          location
        )
      case Typed.TypeApplicationExpression(applicable, typeArgs, location) =>
        val Typed.FunctionType(typeParams, _, _, _) = applicable.tpe
        if (typeParams.size != typeArgs.size) {
          throw InternalError(s"Wrong number of type arguments.", location)
        }
        lazy val translatedApplicable = pushStack(
          translateExpressionArg(applicable, env, typeEnv, nameOpt)
            .translate(translatedExpressions),
          location
        )
        val value: StackLazy[Option[Any]] = StackLazy { stack =>
          translatedApplicable.value.get(location :: stack).map {
            case error: RuntimeError => error
            case f =>
              if (typeParams.isEmpty) f
              else f.asInstanceOf[TypeApplicable](typeArgs.map(_.resolve(typeEnv)))
          }
        }
        val expression = Lazy(Some(Left(StackLazy { stack =>
          Core.TypeApplicationExpression(
            getExpressionArgStrict(translatedApplicable).get(stack),
            typeArgs.map(x => translateType(x, typeEnv)),
            location
          )
        })))
        pushStack(TranslationResult[Any, Some](value, expression), location)
    }
  }

  def pushStack(result: TranslationResult[Any, Some], location: Location) =
    TranslationResult[Any, Some](
      result.value.push(location),
      result.expression
        .push(location)
        .map(e =>
          Some(e.value match {
            case Left(exp) => Left(exp.push(location))
            case Right(StrictOrLazy(exp1, exp2)) =>
              Right(StrictOrLazy(exp1.push(location), exp2.push(location)))
          })
        )
    )

  def mkApplicationExpression(
    applicable: Core.ExpressionArg,
    paramTypes: List[(Typed.Evaluation, Typed.Type)],
    args: ArraySeq[TranslationResult[Any, Some]],
    typeEnv: TypeEnv,
    stack: Stack,
    translatedExpressions: TranslatedExpressions,
    location: Location
  ): Core.ApplicationExpression =
    Core.ApplicationExpression(
      applicable,
      args.zip(paramTypes).map { x =>
        if (x._2._1 != TesslaAST.LazyEvaluation) {
          getExpressionArgStrict(x._1).get(stack)
        } else
          x._1.expression.get(stack).value match {
            case Left(expression) =>
              val id = makeIdentifier(None)
              translatedExpressions.deferredQueue += (() => {
                translatedExpressions.expressions += id -> expression.get(stack)
              })
              Core.ExpressionRef(id, translateType(x._2._2, typeEnv))
            case Right(expressionRef) => expressionRef.translateLazy.get(stack)
          }
      },
      location
    )

  def translateEvaluation(evaluation: Typed.Evaluation): Core.Evaluation = evaluation match {
    case TesslaAST.LazyEvaluation => TesslaAST.LazyEvaluation
    case _                        => TesslaAST.StrictEvaluation
  }

  def translateType(tpe: Typed.Type, typeEnv: TypeEnv) = translateResolvedType(tpe.resolve(typeEnv))

  def translateResolvedType(tpe: Typed.Type): Core.Type = tpe match {
    case Typed.FunctionType(typeParams, paramTypes, resultType, location) =>
      Core.FunctionType(
        typeParams.map(translateIdentifier),
        paramTypes.map(x => (translateEvaluation(x._1), translateResolvedType(x._2))),
        translateResolvedType(resultType),
        location
      )
    case Typed.InstantiatedType(name, typeArgs, location) =>
      Core.InstantiatedType(name, typeArgs.map(translateResolvedType), location)
    case Typed.RecordType(entries, location) =>
      Core.RecordType(
        entries.map(x => (x._1, (translateResolvedType(x._2._1), x._2._2))).toMap,
        location
      )
    case Typed.TypeParam(name, location) =>
      Core.TypeParam(translateIdentifier(name), location)
  }

  private var counter = spec.maxIdentifier

  def makeIdentifier(
    name: Option[String],
    location: Location = Location.unknown
  ): Core.Identifier = {
    counter += 1
    new Core.Identifier(name.map(Ior.Both(_, counter)).getOrElse(Ior.Right(counter)), location)
  }

  def translateIdentifier(identifier: Typed.Identifier) =
    Core.Identifier(identifier.idOrName, identifier.location)
}