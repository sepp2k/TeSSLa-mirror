package de.isp.uni_luebeck.tessla

/**
 * A Layout represents a set of Components and their interconnection. 
 * There may be several available copies of a component.
 *    
 * @author nd
 */
class Layout {
  /**
   * Returns a map assigning to any component an upper 
   * bound on the number of available instances (None 
   * for arbitrary many instances)  
   */
  def componentCopies: Map[Component, Option[Int]]
  
  /**
   * Available components.
   */
  def components: Set[Component]
    
  
}