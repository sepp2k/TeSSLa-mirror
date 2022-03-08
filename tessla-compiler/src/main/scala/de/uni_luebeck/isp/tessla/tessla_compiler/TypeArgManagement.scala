package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._

/**
 * Class for managing type arguments
 * @param resMap Map representing depiction from type arg names to types
 * @param unappliedArgs Types where type application has already been processed but not the expression
 *                      where the types are applied to
 */
case class TypeArgManagement(resMap: Map[Identifier, Type], unappliedArgs: Seq[Type]) {

  /**
   * Process a type application. The new types are stored in [[unappliedArgs]] until the
   * expression they are applied on is hit. If [[unappliedArgs]] already contains types they
   * are overwritten.
   * @param types The types which are applied to the subexpression
   * @return An updated [[TypeArgManagement]]
   */
  def typeApp(types: Seq[Type]): TypeArgManagement = {
    TypeArgManagement(resMap, types)
  }

  /**
   * Processes the sub-expression after a type application and adds the type arg -> type relation to the
   * [[resMap]]. [[unappliedArgs]] is cleared. The types are connected to the arguments in the order
   * they appeared in the type application.
   * @param pars The type params of the sub-expression. If number does not match previous [[typeApp]] call
   *             as much type args as possible are connected to types.
   * @return An updated [[TypeArgManagement]]
   */
  def parsKnown(pars: Seq[Identifier]): TypeArgManagement = {
    TypeArgManagement(resMap.removedAll(pars) ++ pars.zip(unappliedArgs).toMap, Seq())
  }
}

object TypeArgManagement {

  /**
   * Produces a fresh [[TypeArgManagement]] object
   * @return Empty[[TypeArgManagement]]
   */
  def empty: TypeArgManagement = TypeArgManagement(Map(), Seq())
}
