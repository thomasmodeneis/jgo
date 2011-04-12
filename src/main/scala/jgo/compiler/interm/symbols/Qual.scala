/*package jgo.compiler
package interm
package symbols

case class Qual [+S <: Symbol] (pkg: Package, sym: S) extends Symbol

object Qual {
  implicit def qual2regular [S] (q: Qual[S]): S = q.sym
}*/
