package org.brianmckenna.totality

import Macros.{ Zero, Succ, Nat, iter, total }

object Main {
  def main(args: Array[String]) {
    println("Total functional programming in Scala using macros:")

    // 1
    println(total(Succ(Zero)))

    // 2
    println(total(Succ(Succ(Zero))))

    def x[A]: A = x
    // Doesn't compile
    // println(total(x))

    /*println(total {
      def constZero(x: Nat): Nat = Zero
      constZero(Zero)
    })*/

    /*println(total {
      def sum(x: Nat)(y: Nat): Nat = iter(x match {
        case Zero => y
        case Succ(n) => Succ(n)
      })
      sum(Succ(Succ(Zero)))(Succ(Zero))
    })*/

    println(total {
      def sum(x: Nat)(y: Nat): Nat = iter(x match {
        case Zero => y
        case Succ(n) => Succ(n)
      })
      def mult(x: Nat)(y: Nat): Nat = iter(x match {
        case Zero => Zero
        case Succ(n) => sum(y)(n)
      })
      mult(Succ(Succ(Zero)))(Succ(Succ(Succ(Zero))))
    })

    //Goedel.run(
      Goedel.compose(
        Goedel.pair(
          Goedel.id,
          Goedel.curry(
            Goedel.curry(
              Goedel.compose(
                Goedel.pair(
                  Goedel.id,
                  Goedel.compose(
                    Goedel.fst,
                    Goedel.snd
                  )
                ),
                Goedel.iter(
                  Goedel.snd,
                  Goedel.compose(
                    Goedel.snd,
                    Goedel.succ
                  )
                )
              )
            )
          )
        ),
        Goedel.compose(
          Goedel.pair(
            Goedel.id,
            Goedel.curry(
              Goedel.curry(
                Goedel.compose(
                  Goedel.pair(
                    Goedel.id,
                    Goedel.compose(
                      Goedel.fst,
                      Goedel.snd
                    )
                  ),
                  Goedel.iter(
                    Goedel.compose(
                      Goedel.unit,
                      Goedel.zero
                    ),
                    Goedel.compose(
                      Goedel.pair(
                        Goedel.compose(
                          Goedel.pair(
                            Goedel.compose(
                              Goedel.fst,
                              Goedel.compose(
                                Goedel.fst,
                                Goedel.compose(
                                  Goedel.fst,
                                  Goedel.snd
                                )
                              )
                            ),
                            Goedel.compose(
                              Goedel.fst,
                              Goedel.snd
                            )
                          ),
                          Goedel.eval
                        ),
                        Goedel.snd
                      ),
                      Goedel.eval
                    )
                  )
                )
              )
            )
          ),
          Goedel.compose(
            Goedel.pair(
              Goedel.compose(
                Goedel.pair(
                  Goedel.snd,
                  Goedel.compose(
                    Goedel.compose(
                      Goedel.compose(
                        Goedel.unit,
                        Goedel.zero
                      ),
                      Goedel.succ
                    ),
                    Goedel.succ
                  )
                ),
                Goedel.eval
              ),
              Goedel.compose(
                Goedel.compose(
                  Goedel.compose(
                    Goedel.compose(
                      Goedel.unit,
                      Goedel.zero
                    ),
                    Goedel.succ
                  ),
                  Goedel.succ
                ),
                Goedel.succ
              )
            ),
            Goedel.eval
          )
        )
      )
    //)
  }
}
