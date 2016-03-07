package de.uni_luebeck.isp.tessla

// TODO Type unification could be much faster by not using a naive algorithm

case class Env(map: Map[TypeVar, Type]) {
  def apply(v: TypeVar) = map(v)
  def get(v: TypeVar) = map.get(v)

  def updated(v: TypeVar, t: Type) = {
    val update = Env(Map(v -> t))
    Env(map.mapValues(_.substitute(update)).updated(v, t))
  }

  def unify(a: Type, b: Type) =
    a.substitute(this).unify(this, b.substitute(this))
}

object Env {
  def apply(): Env = Env(Map())
}

abstract sealed class Type {
  def unify(env: Env, t: Type): Option[Env]
  def occurs(v: TypeVar): Boolean
  def substitute(env: Env): Type
  def vars: Set[TypeVar]

  def deepCopy: Type = substitute(Env(vars.map(v => v -> new TypeVar).toMap))
}

case class SimpleType(name: String) extends Type {
  def unify(env: Env, t: Type) = {
    t match {
      case SimpleType(otherName) if otherName == name => Some(env)
      case _: TypeVar => t.unify(env, this)
      case _ => None
    }
  }

  def occurs(v: TypeVar): Boolean = false
  def substitute(env: Env) = this
  def vars = Set()
}

case class GenericType(name: String, args: Seq[Type]) extends Type {
  def unify(env: Env, t: Type) = {
    t match {
      case GenericType(otherName, otherArgs)
        if otherName == name && args.length == otherArgs.length =>
        (args zip otherArgs).foldLeft[Option[Env]](Some(env)) {
          case (Some(e), (a1, a2)) => a1.unify(e, a2)
          case _ => None
        }
      case _: TypeVar => t.unify(env, this)
      case _ => None
    }
  }

  def occurs(v: TypeVar): Boolean = args.exists(_.occurs(v))
  def substitute(env: Env) = this.copy(args = args.map(_.substitute(env)))
  def vars = args.flatMap(_.vars).toSet
}

// This is not a case class as all TypeVars are distinct!
class TypeVar extends Type {
  def unify(env: Env, t: Type) = {
    t match {
      case _: TypeVar if t == this => Some(env)
      case _ => if (t.occurs(this)) None else Some(env.updated(this, t))
    }
  }
  def occurs(v: TypeVar) = v == this
  def substitute(env: Env) = env.get(this).getOrElse(this)
  def vars = Set(this)

}
