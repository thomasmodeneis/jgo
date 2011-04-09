package jgo.compiler
package util

class ExpendedException extends IllegalStateException

trait Expendable {
  private var expended = false
  
  protected def expend() {
    errIfExpended()
    expended = true
  }
  
  def isExpended: Boolean = expended
  
  def errIfExpended() = if (isExpended) throw new ExpendedException
  
}
