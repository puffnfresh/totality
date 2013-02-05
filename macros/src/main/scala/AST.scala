package org.brianmckenna.totality

object AST {
  sealed trait Typ
  case object Nat extends Typ
  case object One extends Typ
  case class Prod(a: Typ, b: Typ) extends Typ
  case class Arrow(a: Typ, b: Typ) extends Typ

  sealed trait Term
  case class Var(name: String) extends Term
  case class Let(va: Var, value: Term, body: Term) extends Term
  case class Lambda(va: Var, body: Term) extends Term
  case class Apply(left: Term, right: Term) extends Term
  case class Pair(left: Term, right: Term) extends Term
  case object Unit extends Term
  case class Fst(term: Term) extends Term
  case class Snd(term: Term) extends Term
  case object Zero extends Term
  case class Succ(term: Term) extends Term
  case class Iter(en: Term, ez: Term, va: Var, es: Term) extends Term
  case class Ascription(term: Term, typ: Typ) extends Term
}
