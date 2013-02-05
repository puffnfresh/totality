package org.brianmckenna.totality

import language.higherKinds
import language.implicitConversions

object Goedel {
  import shapeless.{ Poly, Poly1, Poly2, Compose1, Case1Aux, Case2Aux, identity, ~> }

  sealed trait Nat
  case object Z extends Nat
  case class S(n: Nat) extends Nat

  // Need to use Shapeless Poly functions to get type-inference

  val id = identity
  def compose[G <: Poly, F <: Poly](g: G, f: F) = new Compose1[F, G](f, g)

  object unit extends Poly1 {
    implicit def caseA[A] = at[A](_ => ())
  }

  class PairAux[F <: Poly, G <: Poly](f: F, g: G) extends Poly
  object PairAux {
    implicit def caseAB[F <: Poly, G <: Poly, A, B, C](implicit caseF: Poly.Pullback1Aux[F, A, B], caseG: Poly.Pullback1Aux[G, A, C]) = new Case1Aux[PairAux[F, G], A] {
      type R = (B, C)
      val value = (a: A) => (caseF.value.apply(a), caseG.value.apply(a))
    }
  }
  def pair[F <: Poly, G <: Poly](f: F, g: G) = new PairAux(f, g)
  object fst extends Poly1 {
    implicit def caseA[A, B] = at[(A, B)] {
      case (a, _) => a
    }
  }
  object snd extends Poly1 {
    implicit def caseB[A, B] = at[(A, B)] {
      case (_, b) => b
    }
  }

  class CurryAux[F <: Poly](f: F) extends Poly
  object CurryAux {
    implicit def curryCase[F <: Poly, T] = new Case1Aux[CurryAux[F], T] {
      type R = Curry2Aux[F, T]
      val value = (t: T) => new Curry2Aux[F, T](t)
    }
  }
  class Curry2Aux[F <: Poly, T](t: T) {
    def apply[U](u: U)(implicit c: Case2Aux[Curry2Aux[F, T], T, U]): c.R = c(t, u)
  }
  object Curry2Aux {
    implicit def curry2Case[F <: Poly, T, U, V](implicit caseF: Poly.Pullback2Aux[F, T, U, V]) = new Case2Aux[Curry2Aux[F, T], T, U] {
      type R = V
      val value = (t: T, u: U) => caseF.value.apply(t, u)
    }
  }
  def curry[F <: Poly](f: F) = new CurryAux(f)

  object eval extends Poly1 {
    implicit def casePolyT[P <: Poly, T](implicit casePT: Case1Aux[P, T]) = at[(P, T)] {
      case (a, b) => a.apply(b)
    }
  }

  object zero extends Poly1 {
    implicit def caseUnit = at[Unit](_ => Z: Nat)
  }
  object succ extends Poly1 {
    implicit def caseNat = at[Nat](S: Nat => Nat)
  }
  class IterAux[F <: Poly, G <: Poly](z: F, s: G) extends Poly
  object IterAux {
    implicit def caseIter[F <: Poly, G <: Poly, A, B](implicit caseF: Poly.Pullback1Aux[F, A, B], caseG: Poly.Pullback1Aux[G, (A, B), B]) = new Case1Aux[IterAux[F, G], (A, Nat)] {
      type R = B
      val value: ((A, Nat)) => R = {
        case (a, Z) => caseF.value(a)
        case _ => ???
      }
    }
  }
  def iter[F <: Poly, G <: Poly](z: F, s: G) = new IterAux(z, s)

  object run extends Poly1 {
    implicit def runCase[F <: Poly](implicit casePoly: Poly.Pullback1Aux[F, Unit, Nat]) = at[F] { tm =>
      def loop(nat: Nat): Int = nat match {
        case Z => 0
        case S(n) => 1 + loop(n)
      }
      loop(tm.apply(()))
    }
  }

  /*def id[A](a: A): A = a
  def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))

  def unit[A](a: A): Unit = ()

  def pair[A, B, C](f: A => B, g: A => C): A => (B, C) = a => (f(a), g(a))
  def fst[A, B](t: (A, B)): A = t._1
  def snd[A, B](t: (A, B)): B = t._2

  def curry[A, B, C](f: ((A, B)) => C): A => B => C = a => b => f((a, b))
  def eval[A, B](t: ((A => B, A))): B = t._1(t._2)

  def zero(u: Unit): Nat = Z
  def succ(n: Nat): Nat = S(n)
  def iter[A, B](z: A => B, s: ((A, B)) => B): ((A, Nat)) => B = t => t._2 match {
    case Z => z(t._1)
    case S(n) => s((t._1, iter(z, s)((t._1, n))))
  }

  def run(tm: Unit => Nat): Int = {
    def loop(nat: Nat): Int = nat match {
      case Z => 0
      case S(n) => 1 + loop(n)
    }
    loop(tm(()))
  }*/
}
