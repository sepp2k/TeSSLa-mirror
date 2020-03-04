package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TesslaAST.Core
import org.eclipse.tracecompass.ctf.core.event.types.ICompositeDefinition

import scala.collection.immutable.SortedSet

object RuntimeTypeChecker {

  val checker: Map[String, (List[Core.Type], Any) => Option[String]] = Map(
    "Bool" -> ((_, value) => checkAtomic(value.isInstanceOf[Boolean],"Bool", value)),
    "Int" -> ((_, value) => checkAtomic(value.isInstanceOf[BigInt],"Int", value)),
    "Float" -> ((_, value) => checkAtomic(value.isInstanceOf[Double],"Float", value)),
    "String" -> ((_, value) => checkAtomic(value.isInstanceOf[String],"String", value)),
    "CTF" -> ((_, value) => checkAtomic(value.isInstanceOf[ICompositeDefinition],"CTF", value)),
    "Option" -> ((typeArgs, value) => checkAtomic(value.isInstanceOf[Option[_]],"Option", value).orElse(value.asInstanceOf[Option[Any]].flatMap(check(typeArgs.head, _)))),
    "List" -> ((typeArgs, value) => checkAtomic(value.isInstanceOf[List[_]],"List", value).orElse(value.asInstanceOf[List[Any]].flatMap(check(typeArgs.head, _)).headOption)),
    "Set" -> ((typeArgs, value) => checkAtomic(value.isInstanceOf[Set[_]],"Set", value).orElse(value.asInstanceOf[Set[Any]].flatMap(check(typeArgs.head, _)).headOption)),
    "Map" -> ((typeArgs, value) => checkAtomic(value.isInstanceOf[Map[_, _]],"Map", value).orElse(value.asInstanceOf[Map[Any, Any]].flatMap(x => check(typeArgs.head, x._1).orElse(check(typeArgs(1), x._2))).headOption))
  )

  def checkAtomic(check: Boolean, name: String, value: Any) = if (check) {
    None
  } else {
    Some(s"${if(value.isInstanceOf[String]) s""""$value"""" else value} is not of type $name.")
  }

  def check(tpe: Core.Type, value: Any): Option[String] = tpe match {
    case Core.FunctionType(_, _, _, _) =>
      Some("Cannot check function type at runtime.")
    case Core.InstatiatedType(name, typeArgs, _) =>
      checker.get(name).map(f => f(typeArgs, value)).getOrElse(Some(s"No checker found for instatiated type $name."))
    case Core.RecordType(entries, _) =>
      value match {
        case record: RuntimeEvaluator.Record =>
          if (record.entries.keys == entries.keys.map(_.name)) {
            entries.flatMap { entry =>
              check(entry._2, record.entries(entry._1.name))
            }.headOption
          } else {
            val missing = entries.keys.map(_.name).to(SortedSet) -- record.entries.keys
            val notallowed = record.entries.keys.to(SortedSet) -- entries.keys.map(_.name)

            Some(s"Expected $tpe but${if (missing.nonEmpty) missing.mkString(" ", ", ", s" ${if (missing.size == 1) "is" else "are"} missing") else ""}${if (missing.nonEmpty && notallowed.nonEmpty) " and" else ""}${if (notallowed.nonEmpty) notallowed.mkString(" ", ", ", s" ${if (missing.size == 1) "is" else "are"} not allowed") else ""}.")
          }
        case _ => Some(s"Expected $tpe but found ${value.getClass.getName} with value $value.")
      }
    case Core.TypeParam(_, _) =>
      Some("Type parameter must be resolved in runtime type.")
  }

}
