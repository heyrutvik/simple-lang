package com.github.heyrutvik.simple

import Machine.interpreter

class StatementTest extends UnitTest("Statement") {

  implicit val env: Expr.Env = Map('a -> Bool(false), 'b -> Num(10), 'c -> Num(42), 'd -> Num(3), 'e -> Bool(false), 'x -> Num(1))

  "interpret assignment" should "be" in {
    val m = interpreter(Assign('a, Num(10)), env)
    m.env('a) should be (Num(10))
  }

  "interpret if condition" should "be" in {
    val m = interpreter(If(Var('a), Assign('x, Num(10)), Assign('x, Num(42))), env)
    m.expr should be (DoNothing)
  }

  "interpret sequence" should "be" in {
    val m = interpreter(Seq(
      Assign('e, Bool(true)),
      Seq(
        Assign('b, Add(Num(12), Num(15))),
        Assign('c, Mul(Var('b), Var('b)))
      )
    ), env)
    m.env('c) should be (Num((12+15) * (12+15)))
  }

  "interpret while" should "be" in {
    val m = interpreter(While(LessThan(Var('x), Num(5)), Assign('x, Mul(Var('x), Num(3)))), env)
    m.env('x) should be (Num(9))
  }
}
