package jgo.compiler
package util

class FrozenException extends IllegalStateException

trait Freezable {
  private var frozen = false
  
  def freeze() {
    errIfFrozen()
    frozen = true
  }
  
  def isFrozen = frozen
  
  def errIfFrozen() = if (frozen) throw new FrozenException
}
