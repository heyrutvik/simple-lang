package com.rr.kernel

trait Construct[A] {
  def string(a: A): String = a.toString
  def syntax(a: A): String
  def isReducible(a: A): Boolean
  def reduce(a: A): Expr
}

object Construct {

  def apply[A](implicit c: Construct[A]): Construct[A] = c
}

object ConstructSyntax {

  implicit class ConstructOps[A](value: A) {
    def string(implicit c: Construct[A]): String = c.string(value)
    def syntax(implicit c: Construct[A]): String = c.syntax(value)
    def isReducible(implicit c: Construct[A]): Boolean = c.isReducible(value)
    def reduce(implicit c: Construct[A]): Expr = c.reduce(value)
  }
}

object ConstructImplicits {

  /**
    * TODO: SyntaxConstruct Must be Automated
    */
  implicit val exprConstruct: Construct[Expr] = new Construct[Expr] {
    override def syntax(a: Expr): String = a match {
      case n: Num => numberConstruct.syntax(n)
      case a: Add => addConstruct.syntax(a)
      case m: Mul => mulConstruct.syntax(m)
    }
    override def isReducible(a: Expr): Boolean = a match {
      case n: Num => numberConstruct.isReducible(n)
      case a: Add => addConstruct.isReducible(a)
      case m: Mul => mulConstruct.isReducible(m)
    }
    override def reduce(a: Expr): Expr = a match {
      case n: Num => numberConstruct.reduce(n)
      case a: Add => addConstruct.reduce(a)
      case m: Mul => mulConstruct.reduce(m)
    }
  }

  implicit val numberConstruct: Construct[Num] = new Construct[Num] {
    override def syntax(a: Num): String = s"${a.value.toString}"
    override def isReducible(a: Num): Boolean = false
    override def reduce(a: Num): Num = a
  }

  implicit def addConstruct(implicit cs: Construct[Expr]): Construct[Add] = new Construct[Add] {
    override def syntax(a: Add): String = s"${cs.syntax(a.left)} + ${cs.syntax(a.right)}"
    override def isReducible(a: Add): Boolean = true
    override def reduce(a: Add): Expr = a match {
      case Add(left, right) if cs.isReducible(left) => Add(cs.reduce(left), right)
      case Add(left, right) if cs.isReducible(right) => Add(left, cs.reduce(right))
      case Add(Num(x), Num(y)) => Num(x + y)
    }
  }

  implicit def mulConstruct(implicit cs: Construct[Expr]): Construct[Mul] = new Construct[Mul] {
    override def syntax(a: Mul): String = s"${cs.syntax(a.left)} * ${cs.syntax(a.right)}"
    override def isReducible(a: Mul): Boolean = true
    override def reduce(a: Mul): Expr = a match {
      case Mul(left, right) if cs.isReducible(left) => Mul(cs.reduce(left), right)
      case Mul(left, right) if cs.isReducible(right) => Mul(left, cs.reduce(right))
      case Mul(Num(x), Num(y)) => Num(x * y)
    }
  }
}