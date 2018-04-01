package com.rr.kernel

trait Construct[A] {
  type S <: Syntax
  def string(a: A): String = a.toString
  def syntax(a: A): String
  def isReducible(a: A): Boolean
  def reduce(a: A): S
}

object Construct {

  def apply[A](implicit c: Construct[A]) = c
}

object ConstructSyntax {

  implicit class ConstructOps[A](value: A) {
    def string(implicit c: Construct[A]): String = c.string(value)
    def syntax(implicit c: Construct[A]): String = c.syntax(value)
    def isReducible(implicit c: Construct[A]): Boolean = c.isReducible(value)
    def reduce(implicit c: Construct[A]): c.S = c.reduce(value)
  }
}

object ConstructImplicits {

  implicit val numberConstruct = new Construct[Num] {
    override type S = Num
    override def syntax(a: Num): String = s"${a.value.toString}"
    override def isReducible(a: Num): Boolean = false
    override def reduce(a: Num): S = a
  }

  implicit def addConstruct[A, B](implicit lc: Construct[A], rc: Construct[B]) = new Construct[Add[A, B]] {
    override type S <: Syntax
    override def syntax(a: Add[A, B]): String = s"${lc.syntax(a.left)} + ${rc.syntax(a.right)}"
    override def isReducible(a: Add[A, B]): Boolean = true
    override def reduce(a: Add[A, B]): S = a match {
      case Add(left, right) if lc.isReducible(left) => Add(lc.reduce(left), right)
      case Add(left, right) if rc.isReducible(right) => Add(left, rc.reduce(right))
      case Add(Num(x), Num(y)) => Num(x + y)
    }
  }

  implicit def mulConstruct[A, B](implicit lc: Construct[A], rc: Construct[B]) = new Construct[Mul[A, B]] {
    override type S <: Syntax
    override def syntax(a: Mul[A, B]): String = s"${lc.syntax(a.left)} * ${rc.syntax(a.right)}"
    override def isReducible(a: Mul[A, B]): Boolean = true
    override def reduce(a: Mul[A, B]): S = a match {
      case Mul(left, right) if lc.isReducible(left) => Mul(lc.reduce(left), right)
      case Mul(left, right) if rc.isReducible(right) => Mul(left, rc.reduce(right))
      case Mul(Num(x), Num(y)) => Num(x * y)
    }
  }
}