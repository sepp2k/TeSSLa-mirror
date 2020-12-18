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

package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.core.ConstantEvaluator
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import org.eclipse.tracecompass.ctf.core.event.IEventDefinition

import scala.collection.SortedSet

/**
 * Performs type checking on runtime values.
 */

object RuntimeTypeChecker {

  val checker: Map[String, (List[Core.Type], Any) => Option[String]] = Map(
    "Bool" -> ((_, value) => checkAtomic(value.isInstanceOf[Boolean], "Bool", value)),
    "Int" -> ((_, value) => checkAtomic(value.isInstanceOf[BigInt], "Int", value)),
    "Float" -> ((_, value) => checkAtomic(value.isInstanceOf[Double], "Float", value)),
    "String" -> ((_, value) => checkAtomic(value.isInstanceOf[String], "String", value)),
    "CTF" -> ((_, value) => checkAtomic(value.isInstanceOf[IEventDefinition], "CTF", value)),
    "Option" -> ((typeArgs, value) =>
      checkAtomic(value.isInstanceOf[Option[_]], "Option", value)
        .orElse(value.asInstanceOf[Option[Any]].flatMap(check(typeArgs.head, _)))
    ),
    "List" -> ((typeArgs, value) =>
      checkAtomic(value.isInstanceOf[List[_]], "List", value)
        .orElse(value.asInstanceOf[List[Any]].flatMap(check(typeArgs.head, _)).headOption)
    ),
    "Set" -> ((typeArgs, value) =>
      checkAtomic(value.isInstanceOf[Set[_]], "Set", value)
        .orElse(value.asInstanceOf[Set[Any]].flatMap(check(typeArgs.head, _)).headOption)
    ),
    "Map" -> ((typeArgs, value) =>
      checkAtomic(value.isInstanceOf[Map[_, _]], "Map", value).orElse(
        value
          .asInstanceOf[Map[Any, Any]]
          .flatMap(x => check(typeArgs.head, x._1).orElse(check(typeArgs(1), x._2)))
          .headOption
      )
    )
  )

  private def checkAtomic(check: Boolean, name: String, value: Any) = if (check) {
    None
  } else {
    Some(s"${if (value.isInstanceOf[String]) s""""$value"""" else value} is not of type $name.")
  }

  def check(tpe: Core.Type, value: Any): Option[String] = tpe match {
    case Core.FunctionType(_, _, _, _) =>
      Some("Cannot check function type at runtime.")
    case Core.InstantiatedType(name, typeArgs, _) =>
      checker
        .get(name)
        .map(f => f(typeArgs, value))
        .getOrElse(Some(s"No checker found for instatiated type $name."))
    case Core.RecordType(entries, _) =>
      value match {
        case record: ConstantEvaluator.Record =>
          if (record.entries.keys == entries.keys) {
            entries.flatMap { entry =>
              check(entry._2._1, record.entries(entry._1))
            }.headOption
          } else {
            val missing = entries.keys.to(SortedSet) diff record.entries.keys.toSet
            val notallowed = record.entries.keys.to(SortedSet) diff entries.keys.toSet

            Some(s"Expected $tpe but${if (missing.nonEmpty)
              missing.mkString(" ", ", ", s" ${if (missing.size == 1) "is" else "are"} missing")
            else ""}${if (missing.nonEmpty && notallowed.nonEmpty) " and" else ""}${if (notallowed.nonEmpty)
              notallowed.mkString(" ", ", ", s" ${if (missing.size == 1) "is" else "are"} not allowed")
            else ""}.")
          }
        case _ => Some(s"Expected $tpe but found ${value.getClass.getName} with value $value.")
      }
    case Core.TypeParam(_, _) =>
      Some("Type parameter must be resolved in runtime type.")
  }

}
