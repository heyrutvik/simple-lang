package com.github.heyrutvik.simple

import Expr.Env

trait Construct[A] {
  def string(a: A): String = a.toString
  def syntax(a: A): String
  def isReducible(a: A): Boolean
  def reduce(a: A)(implicit env: Expr.Env): Product
}

object Construct {

  def apply[A](implicit c: Construct[A]): Construct[A] = c
}

object ConstructSyntax {

  implicit class ConstructOps[A](value: A) {
    def string(implicit c: Construct[A]): String = c.string(value)
    def syntax(implicit c: Construct[A]): String = c.syntax(value)
    def isReducible(implicit c: Construct[A]): Boolean = c.isReducible(value)
    def reduce(implicit env: Expr.Env, c: Construct[A]): Product = c.reduce(value)(env)
  }
}

object ConstructImplicits { self =>

  /**
    * TODO: These Methods Must be Automated
    */

  private def syntax(a: Expr): String = a match {
    case n: Num => numberConstruct.syntax(n)
    case a: Add => addConstruct.syntax(a)
    case m: Mul => mulConstruct.syntax(m)
    case b: Bool => boolConstruct.syntax(b)
    case lt: LessThan => lessThanConstruct.syntax(lt)
    case v: Var => varConstruct.syntax(v)
    case dn: DoNothing.type => doNothingConstruct.syntax(dn)
    case as: Assign => assignConstruct.syntax(as)
    case i: If => ifConstruct.syntax(i)
    case s: Seq => seqConstruct.syntax(s)
  }
  private def isReducible(a: Expr): Boolean = a match {
    case n: Num => numberConstruct.isReducible(n)
    case a: Add => addConstruct.isReducible(a)
    case m: Mul => mulConstruct.isReducible(m)
    case b: Bool => boolConstruct.isReducible(b)
    case lt: LessThan => lessThanConstruct.isReducible(lt)
    case v: Var => varConstruct.isReducible(v)
    case dn: DoNothing.type => doNothingConstruct.isReducible(dn)
    case as: Assign => assignConstruct.isReducible(as)
    case i: If => ifConstruct.isReducible(i)
    case s: Seq => seqConstruct.isReducible(s)
  }
  private def reduce(a: Expr)(implicit env: Expr.Env): Product = a match {
    case n: Num => numberConstruct.reduce(n)
    case a: Add => addConstruct.reduce(a)
    case m: Mul => mulConstruct.reduce(m)
    case b: Bool => boolConstruct.reduce(b)
    case lt: LessThan => lessThanConstruct.reduce(lt)
    case v: Var => varConstruct.reduce(v)
    case dn: DoNothing.type => doNothingConstruct.reduce(dn)
    case as: Assign => assignConstruct.reduce(as)
    case i: If => ifConstruct.reduce(i)
    case s: Seq => seqConstruct.reduce(s)
  }

  implicit val productConstruct: Construct[Product] = new Construct[Product] {
    override def syntax(a: Product): String = self.syntax(a.expr)
    override def isReducible(a: Product): Boolean = self.isReducible(a.expr)
    override def reduce(a: Product)(implicit env: Expr.Env): Product = self.reduce(a.expr)
  }

  implicit val exprConstruct: Construct[Expr] = new Construct[Expr] {
    override def syntax(a: Expr): String = self.syntax(a)
    override def isReducible(a: Expr): Boolean = self.isReducible(a)
    override def reduce(a: Expr)(implicit env: Expr.Env): Product = self.reduce(a)
  }

  implicit val numberConstruct: Construct[Num] = new Construct[Num] {
    override def syntax(a: Num): String = s"${a.value.toString}"
    override def isReducible(a: Num): Boolean = false
    override def reduce(a: Num)(implicit env: Expr.Env): Product = Product(a)
  }

  implicit def addConstruct(implicit cs: Construct[Expr]): Construct[Add] = new Construct[Add] {
    override def syntax(a: Add): String = s"${cs.syntax(a.left)} + ${cs.syntax(a.right)}"
    override def isReducible(a: Add): Boolean = true
    override def reduce(a: Add)(implicit env: Expr.Env): Product = a match {
      case Add(left, right) if cs.isReducible(left) => Product(Add(cs.reduce(left)(env).expr, right))
      case Add(left, right) if cs.isReducible(right) => Product(Add(left, cs.reduce(right)(env).expr))
      case Add(Num(x), Num(y)) => Product(Num(x + y))
    }
  }

  implicit def mulConstruct(implicit cs: Construct[Expr]): Construct[Mul] = new Construct[Mul] {
    override def syntax(a: Mul): String = s"${cs.syntax(a.left)} * ${cs.syntax(a.right)}"
    override def isReducible(a: Mul): Boolean = true
    override def reduce(a: Mul)(implicit env: Expr.Env): Product = a match {
      case Mul(left, right) if cs.isReducible(left) => Product(Mul(cs.reduce(left)(env).expr, right))
      case Mul(left, right) if cs.isReducible(right) => Product(Mul(left, cs.reduce(right)(env).expr))
      case Mul(Num(x), Num(y)) => Product(Num(x * y))
    }
  }

  implicit val boolConstruct: Construct[Bool] = new Construct[Bool] {
    override def syntax(a: Bool): String = s"${a.value.toString}"
    override def isReducible(a: Bool): Boolean = false
    override def reduce(a: Bool)(implicit env: Expr.Env): Product = Product(a)
  }

  implicit def lessThanConstruct(implicit cs: Construct[Expr]): Construct[LessThan] = new Construct[LessThan] {
    override def syntax(a: LessThan): String = s"${cs.syntax(a.left)} < ${cs.syntax(a.right)}"
    override def isReducible(a: LessThan): Boolean = true
    override def reduce(a: LessThan)(implicit env: Expr.Env): Product = a match {
      case LessThan(left, right) if cs.isReducible(left) => Product(LessThan(cs.reduce(left)(env).expr, right))
      case LessThan(left, right) if cs.isReducible(right) => Product(LessThan(left, cs.reduce(right)(env).expr))
      case LessThan(Num(x), Num(y)) => Product(Bool(x < y))
    }
  }

  implicit val varConstruct: Construct[Var] = new Construct[Var] {
    override def syntax(a: Var): String = s"${a.name.toString}"
    override def isReducible(a: Var): Boolean = true
    override def reduce(a: Var)(implicit env: Expr.Env): Product = Product(env(a.name))
  }

  implicit val doNothingConstruct: Construct[DoNothing.type] = new Construct[DoNothing.type] {
    override def syntax(a: DoNothing.type): String = s"do-nothing"
    override def isReducible(a: DoNothing.type): Boolean = false
    override def reduce(a: DoNothing.type)(implicit env: Expr.Env): Product = Product(a, env)
  }

  implicit def assignConstruct(implicit cs: Construct[Expr]): Construct[Assign] = new Construct[Assign] {
    override def syntax(a: Assign): String = s"${a.name} = ${cs.syntax(a.expr)}"
    override def isReducible(a: Assign): Boolean = true
    override def reduce(a: Assign)(implicit env: Env): Product = {
      if (cs.isReducible(a.expr))
        Product(Assign(a.name, cs.reduce(a.expr)(env).expr), env)
      else
        Product(DoNothing, env + (a.name -> a.expr))
    }
  }

  implicit def ifConstruct(implicit cs: Construct[Expr]): Construct[If] = new Construct[If] {
    override def syntax(a: If): String = s"if (${a.condition}) { ${a.consequence} } else { ${a.alternative} }"
    override def isReducible(a: If): Boolean = true
    override def reduce(a: If)(implicit env: Env): Product = {
      if (cs.isReducible(a.condition))
        Product(If(cs.reduce(a.condition).expr, a.consequence, a.alternative), env)
      else {
        a.condition match {
          case Bool(true) => Product(a.consequence, env)
          case Bool(false) => Product(a.alternative, env)
          case _ => throw new Exception("condition value must be boolean")
        }
      }
    }
  }

  implicit def seqConstruct(implicit cs: Construct[Expr]): Construct[Seq] = new Construct[Seq] {
    override def syntax(a: Seq): String = s"{${cs.syntax(a.first)}; ${cs.syntax(a.second)}}"
    override def isReducible(a: Seq): Boolean = true
    override def reduce(a: Seq)(implicit env: Env): Product = {
      println(s"---env: $env")
      a.first match {
        case _: DoNothing.type => Product(a.second, env)
        case _ => {
          val Product(reducedExpr, reducedEnv) = cs.reduce(a.first)
          Product(Seq(reducedExpr, a.second), reducedEnv)
        }
      }
    }
  }
}