package com.github.heyrutvik.simple

import ConstructImplicits._
import ConstructSyntax._
import Machine.interpreter

class StatementTest extends UnitTest("Statement") {

  implicit val env: Expr.Env = Map('a -> Bool(false), 'b -> Num(10), 'c -> Num(42), 'd -> Num(3), 'e -> Bool(false))

  "assignment" should "be" in {
    val m = interpreter(Assign('a, Num(10)))
    m.env('a) should be (Num(10))
  }

  "if condition" should "be" in {
    val m = interpreter(If(Var('a), Assign('x, Num(10)), Assign('x, Num(42))))
    m.expr should be (DoNothing)
  }

  "sequence" should "be" in {
    val m = interpreter(Seq(
      Assign('e, Bool(true)),
      Seq(
        Assign('b, Add(Num(12), Num(15))),
        Assign('c, Mul(Var('b), Var('b)))
      )
    ))
    m.env('c) should be (Num((12+15) * (12+15)))
  }
}
