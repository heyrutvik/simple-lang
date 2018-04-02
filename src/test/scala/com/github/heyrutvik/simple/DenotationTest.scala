package com.github.heyrutvik.simple

import com.github.heyrutvik.simple.ConstructImplicits._
import com.github.heyrutvik.simple.ConstructSyntax._

class DenotationTest extends UnitTest("Denotation") {

  "javascript number" should "be" in {
    Num(10).toJs should be ("e => 10")
  }

  "javascript boolean" should "be" in {
    Bool(false).toJs should be ("e => false")
  }

  "javascript variable" should "be" in {
    Var('x).toJs should be ("e => e.get('x')")
  }

  "javascript add" should "be" in {
    Add(Var('x), Num(5)).toJs should be ("e => (e => e.get('x'))(e) + (e => 5)(e)")
  }

  "javascript mul" should "be" in {
    Mul(Var('x), Num(5)).toJs should be ("e => (e => e.get('x'))(e) * (e => 5)(e)")
  }

  "javascript lessthan" should "be" in {
    LessThan(Var('x), Num(5)).toJs should be ("e => (e => e.get('x'))(e) < (e => 5)(e)")
  }

  "javascript assignment" should "be" in {
    Assign('a, Num(123)).toJs should be ("e => e.set('a', (e => 123)(e))")
  }

  "javascript if condition" should "be" in {
    If(Var('a), Assign('x, Num(10)), Assign('x, Num(42))).toJs should be
    ("e => { if ((e => e.get('a'))(e)) { return ((e.set('a', (e => 10)(e)))(e)) } else { return ((e.set('a', (e => 42)(e))(e)) } }")
  }

  "javascript sequence" should "be" in {
    val s = Seq(
      Assign('e, Bool(true)),
      Seq(
        Assign('b, Add(Num(12), Num(15))),
        Assign('c, Mul(Var('b), Var('b)))
      )
    )
    s.toJs should be ("e => { return ((e => { return ((e => e.set('c', (e => (e => e.get('b'))(e) * (e => e.get('b'))(e))(e))))((e => e.set('b', (e => (e => 12)(e) + (e => 15)(e))(e)))(e)) }))((e => e.set('e', (e => true)(e)))(e)) }")
  }

  "javascript while" should "be" in {
    val w = While(LessThan(Var('x), Num(5)), Assign('x, Mul(Var('x), Num(3))))
    w.toJs should be ("e => { while ((e => (e => e.get('x'))(e) < (e => 5)(e))(e)) { e = (e => e.set('x', (e => (e => e.get('x'))(e) * (e => 3)(e))(e)))(e) } return e;}")
  }
}
