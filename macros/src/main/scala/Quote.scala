package org.brianmckenna.totality

import reflect.macros.Context

trait HasContext[C <: Context] {
  val c: C
}

trait Quote[C <: Context] extends HasContext[C] {
  import c.Tree
  import c.universe.{ Apply, Ident, Select, reify }

  def id =
    Select(Ident("Goedel"), "id")
  def compose(f: Tree, g: Tree) =
    Apply(Select(Ident("Goedel"), "compose"), List(f, g))
  def unit =
    Select(Ident("Goedel"), "unit")

  def fst =
    Select(Ident("Goedel"), "fst")
  def snd =
    Select(Ident("Goedel"), "snd")
  def pair(f: Tree, g: Tree) =
    Apply(Select(Ident("Goedel"), "pair"), List(f, g))
  /*def prod(f: Tree, g: Tree) =
    Apply(Apply(reify(Goedel.prod).tree, List(f)), List(g))*/

  def curry(f: Tree) =
    Apply(Select(Ident("Goedel"), "curry"), List(f))
  def eval =
    Select(Ident("Goedel"), "eval")

  def zero =
    Select(Ident("Goedel"), "zero")
  def succ =
    Select(Ident("Goedel"), "succ")
  def iter(z: Tree, s: Tree) =
    Apply(Select(Ident("Goedel"), "iter"), List(z, s))
}

trait QuoteFind[C <: Context] extends Quote[C] {
  import c.Tree

  import AST.{ Var, Typ }

  def find(va: Var, varTyps: List[(Var, Typ)]): Option[Tree] = varTyps match {
    case Nil => None
    case (x, _) :: _ if x == va => Some(snd)
    case (_, _) :: xs => find(va, xs) flatMap { found =>
      Some(compose(fst, found))
    }
  }
}
