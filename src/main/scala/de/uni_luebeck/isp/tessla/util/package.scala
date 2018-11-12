package de.uni_luebeck.isp.tessla

package object util {
  /**
    * `optionIf(cond) {foo}` evaluates `foo` and returns its value wrapped in a `Some` if `cond`
    * is true; otherwise `None` is returned.
    */
  def optionIf[T](condition: Boolean)(thenCase: =>T): Option[T] = {
    if (condition) Some(thenCase) else None
  }
}
