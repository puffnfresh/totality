package org.brianmckenna.totality

import language.experimental.macros

import reflect.macros.Context

object Macros {
  def total(a: Any) = macro totalImpl

  type Nat = Any
  val Zero: Any = 0
  case class Succ(a: Any)
  def iter(a: Any) = ???

  def totalImpl(c: Context)(a: c.Expr[Any]): c.Expr[Int] = {
    import c.universe._

    val E = new Elaborate[c.type](c)

    def treeType(typeTree: TypeTree) = typeTree.original match {
      case Select(_, tn) if tn.decoded == "Nat" => AST.Nat
    }

    def term(tree: c.Tree): AST.Term = tree match {
      case Literal(Constant(_: Unit)) =>
        AST.Unit

      case Apply(Select(Select(_, tn1), tn2), arg :: Nil) if tn1.decoded == "Succ" && tn2.decoded == "apply" =>
        AST.Succ(term(arg))

      case Apply(Select(_, tn), Match(selector, cases) :: Nil) if tn.decoded == "iter" =>

        def collectZero(cas: List[c.Tree]): AST.Term = cas match {
          case Nil =>
            sys.error("Not supplied a Zero to pattern match")

          case CaseDef(Select(_, tn2), _, body) :: _ if tn2.decoded == "Zero" =>
            term(body)

          case _ :: cs =>
            collectZero(cs)
        }

        def collectSucc(cas: List[c.Tree]): (AST.Var, AST.Term) = cas match {
          case Nil =>
            sys.error("Not supplied a Succ to pattern match")

          case CaseDef(Apply(_: TypeTree, Bind(name, _) :: Nil), _, body) :: _ =>
            (AST.Var(name.decoded), term(body))

          case _ :: cs =>
            collectSucc(cs)
        }

        val zTerm = collectZero(cases)
        val (sVar, sTerm) = collectSucc(cases)

        AST.Iter(term(selector), zTerm, sVar, sTerm)

      case Apply(name, arg :: Nil) =>
        AST.Apply(term(name), term(arg))

      case Select(_, tn) if tn.decoded == "Zero" =>
        AST.Zero

      case Ident(tn) =>
        AST.Var(tn.decoded)

      case Block(statement :: statements, expr) =>
        statement match {
          case DefDef(_, name, _, args, retTypeTree: TypeTree, body) =>
            val init: (AST.Term, AST.Typ) = (term(body), treeType(retTypeTree))

            val (program, arrowType) = args.foldRight(init) { (arg, accum) =>
              val prog = accum._1
              val typ = accum._2

              arg match {
                case ValDef(_, name, typeTree: TypeTree, _) :: Nil =>
                  (AST.Lambda(AST.Var(name.decoded), prog), AST.Arrow(treeType(typeTree), typ))
                case _ => ???
              }
            }

            AST.Let(AST.Var(name.decoded), AST.Ascription(program, arrowType), term(Block(statements, expr)))
        }

      case Block(Nil, expr) =>
        term(expr)
    }

    val ast = term(a.tree)
    println(ast)
    E.run(E.synth(ast)) match {
      case E.Done((a, _)) =>
        println(Apply(Select(Ident("Goedel"), "run"), List(a)))
        c.Expr(Apply(Select(Ident("Goedel"), "run"), List(a)))
      case E.Error(s) => sys.error(s)
    }
  }
}
