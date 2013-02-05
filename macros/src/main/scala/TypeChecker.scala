package org.brianmckenna.totality

import reflect.macros.Context

trait Environment[C <: Context] extends HasContext[C] {
  import AST.{ Var, Typ }

  case class Env(varTyps: Map[Var, Typ])

  def find(va: Var, varTypes: List[(Var, Typ)]): Option[c.Tree]

  sealed trait Result[A]
  case class Error[A](e: String) extends Result[A]
  case class Done[A](a: A) extends Result[A]

  case class EnvResult[A](f: Env => Result[A]) {
    def flatMap[B](fb: A => EnvResult[B]) =
      EnvResult[B](env => f(env) match {
        case Done(a) => fb(a).f(env)
        case Error(e) => Error(e)
      })

    def map[B](fb: A => B) =
      EnvResult[B](env => f(env) match {
        case Done(a) => Done(fb(a))
        case Error(e) => Error(e)
      })
  }
  object EnvResult {
    def pure[A](a: A) = EnvResult(_ => Done(a))
  }

  type T[A] = Env => Result[A]

  def withHyp[A](hyp: (Var, Typ), cmd: EnvResult[A]) =
    EnvResult(env => cmd.f(Env(env.varTyps + hyp)))

  def lookup(va: Var): EnvResult[(c.Tree, Typ)] = EnvResult { env =>
    find(va, env.varTyps.toList) match {
      case Some(tree) =>
        Done((tree, env.varTyps(va)))
      case None =>
        Error("Tree not found in Environment")
    }
  }

  def run[A](cmd: EnvResult[A]) = cmd.f(Env(Map.empty))
}

class Elaborate[C <: Context](val c: C) extends QuoteFind[C] with Environment[C] {
  import AST._

  def check(term: Term, typ: Typ): EnvResult[c.Tree] = (term, typ) match {
    case (Lambda(va, body), Arrow(typ1, typ2)) => withHyp((va, typ1), for {
      t <- check(body, typ2)
    } yield curry(t))
    case (Lambda(_, _), _) => sys.error("Lambda needs Arrow type")

    case (Unit, One) => EnvResult.pure(unit)
    case (Unit, _) => sys.error("Unit needs One type")

    case (Pair(term1, term2), Prod(typ1, typ2)) => for {
      t1 <- check(term1, typ1)
      t2 <- check(term2, typ2)
    } yield pair(t1, t2)
    case (Pair(_, _), _) => sys.error("Pair needs Prod type")

    case (Iter(en, ez, x, es), typ) => for {
      tn <- check(en, Nat)
      tz <- check(ez, typ)
      ts <- withHyp((x, typ), check(es, typ))
    } yield compose(pair(id, tn), iter(tz, ts))

    case (Let(va, value, body), typ2) => for {
      (t1, typ1) <- synth(value)
      t2 <- withHyp((va, typ1), check(body, typ2))
    } yield compose(pair(id, t1), t2)

    case (_, _) => for {
      (t, typ2) <- synth(term)
      _ = assert(typ == typ2)
    } yield t
  }

  def synth(term: Term): EnvResult[(c.Tree, Typ)] = term match {
    case va@Var(_) => lookup(va)
    case Zero => EnvResult.pure((compose(unit, zero), Nat))
    case Succ(term) => for {
      t1 <- check(term, Nat)
    } yield (compose(t1, succ), Nat)
    case Apply(left, right) => for {
      leftSynth <- synth(left)
      (t1, Arrow(typ2, typ)) = leftSynth
      t2 <- check(right, typ2)
    } yield (compose(pair(t1, t2), eval), typ)
    case Fst(term) => for {
      termSynth <- synth(term)
      (tt, Prod(typ1, _)) = termSynth
    } yield (compose(tt, fst), typ1)
    case Snd(term) => for {
      termSynth <- synth(term)
      (tt, Prod(_, typ2)) = termSynth
    } yield (compose(tt, snd), typ2)
    case Let(va, value, body) => for {
      (t1, typ1) <- synth(value)
      (t2, typ2) <- withHyp((va, typ1), synth(body))
    } yield (compose(pair(id, t1), t2), typ2)
    case Ascription(term, typ) => for {
      t <- check(term, typ)
    } yield (t, typ)
    case _ => sys.error("Checking term in synth position")
  }
}
