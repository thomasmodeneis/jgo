package jgo.compiler
package interm
package symbols

case class Qual [+S <: Symbol] (pkg: Package, sym: S) extends Symbol

implicit def qual2regular [S] (q: Qual[S]): S = qual.sym
