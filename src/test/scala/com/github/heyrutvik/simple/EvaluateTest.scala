package com.github.heyrutvik.simple

import ConstructImplicits._
import ConstructSyntax._

class EvaluateTest extends UnitTest("Statement") {

  implicit val env: Expr.Env = Map('a -> Bool(false), 'b -> Num(10), 'c -> Num(42), 'd -> Num(3), 'e -> Bool(false), 'x -> Num(1))

  "assignment" should "be" in {

    Assign('a, Num(10)).evaluate(env).env.get('a) should be (Num(10))
  }

  "if condition" should "be" in {
    If(Var('a), Assign('x, Num(10)), Assign('x, Num(42))).evaluate(env).env.get('x) should be (Num(42))
  }

  "sequence" should "be" in {
    val s = Seq(
      Assign('e, Bool(true)),
      Seq(
        Assign('b, Add(Num(12), Num(15))),
        Assign('c, Mul(Var('b), Var('b)))
      )
    )
    s.evaluate(env).env.get('c) should be (Num((12+15)*(12+15)))
  }

  "while" should "be" in {
    val w = While(LessThan(Var('x), Num(5)), Assign('x, Mul(Var('x), Num(3))))
    w.evaluate(env).env.get('x) should be (Num(9))
  }
}
