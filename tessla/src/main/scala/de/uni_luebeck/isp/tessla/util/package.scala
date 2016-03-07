package de.uni_luebeck.isp.tessla

package object util {
  def unreachable: Nothing = {
    // We're not using assert here as we can't return a value if asserts are off
    throw new AssertionError("code marked as unreachable was reached")
  }
}
