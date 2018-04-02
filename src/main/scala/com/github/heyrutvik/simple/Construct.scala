package com.github.heyrutvik.simple

import Expr.Env

trait Construct[A] {
  def string(a: A): String = a.toString
  def syntax(a: A): String
  def isReducible(a: A): Boolean
  def reduce(a: A)(implicit env: Expr.Env): Product
  def evaluate(a: A)(implicit env: Expr.Env): Product
}

object Construct {

  def apply[A](implicit c: Construct[A]): Construct[A] = c
}

object ConstructSyntax {

  implicit class ConstructOps[A](value: A) {
    def string(implicit c: Construct[A]): String = c.string(value)
    def syntax(implicit c: Construct[A]): String = c.syntax(value)
    def isReducible(implicit c: Construct[A]): Boolean = c.isReducible(value)
    def reduce(env: Expr.Env)(implicit c: Construct[A]): Product = c.reduce(value)(env)
    def evaluate(env: Expr.Env)(implicit c: Construct[A]): Product = c.evaluate(value)(env)
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
    case w: While => whileConstruct.syntax(w)
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
    case w: While => whileConstruct.isReducible(w)
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
    case w: While => whileConstruct.reduce(w)
  }
  private def evaluate(a: Expr)(implicit env: Expr.Env): Product = a match {
    case n: Num => numberConstruct.evaluate(n)
    case a: Add => addConstruct.evaluate(a)
    case m: Mul => mulConstruct.evaluate(m)
    case b: Bool => boolConstruct.evaluate(b)
    case lt: LessThan => lessThanConstruct.evaluate(lt)
    case v: Var => varConstruct.evaluate(v)
    case dn: DoNothing.type => doNothingConstruct.evaluate(dn)
    case as: Assign => assignConstruct.evaluate(as)
    case i: If => ifConstruct.evaluate(i)
    case s: Seq => seqConstruct.evaluate(s)
    case w: While => whileConstruct.evaluate(w)
  }

  implicit val productConstruct: Construct[Product] = new Construct[Product] {
    override def syntax(a: Product): String = self.syntax(a.expr)
    override def isReducible(a: Product): Boolean = self.isReducible(a.expr)
    override def reduce(a: Product)(implicit env: Expr.Env): Product = self.reduce(a.expr)
    override def evaluate(a: Product)(implicit env: Env): Product = self.evaluate(a.expr)
  }

  implicit val exprConstruct: Construct[Expr] = new Construct[Expr] {
    override def syntax(a: Expr): String = self.syntax(a)
    override def isReducible(a: Expr): Boolean = self.isReducible(a)
    override def reduce(a: Expr)(implicit env: Expr.Env): Product = self.reduce(a)
    override def evaluate(a: Expr)(implicit env: Env): Product = self.evaluate(a)
  }

  implicit val numberConstruct: Construct[Num] = new Construct[Num] {
    override def syntax(a: Num): String = s"${a.value.toString}"
    override def isReducible(a: Num): Boolean = false
    override def reduce(a: Num)(implicit env: Expr.Env): Product = Product(a)
    override def evaluate(a: Num)(implicit env: Env): Product = Product(a)
  }

  implicit def addConstruct(implicit cs: Construct[Expr]): Construct[Add] = new Construct[Add] {
    override def syntax(a: Add): String = s"${cs.syntax(a.left)} + ${cs.syntax(a.right)}"
    override def isReducible(a: Add): Boolean = true
    override def reduce(a: Add)(implicit env: Expr.Env): Product = a match {
      case Add(left, right) if cs.isReducible(left) => Product(Add(cs.reduce(left)(env).expr, right))
      case Add(left, right) if cs.isReducible(right) => Product(Add(left, cs.reduce(right)(env).expr))
      case Add(Num(x), Num(y)) => Product(Num(x + y))
    }
    override def evaluate(a: Add)(implicit env: Env): Product = {
      (cs.evaluate(a.left).expr, cs.evaluate(a.right).expr) match {
        case (Num(x), Num(y)) => Product(Num(x + y))
        case _ => throw new Exception("evaluate: something went wrong")
      }
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
    override def evaluate(a: Mul)(implicit env: Env): Product = {
      (cs.evaluate(a.left).expr, cs.evaluate(a.right).expr) match {
        case (Num(x), Num(y)) => Product(Num(x * y))
        case _ => throw new Exception("evaluate: something went wrong")
      }
    }
  }

  implicit val boolConstruct: Construct[Bool] = new Construct[Bool] {
    override def syntax(a: Bool): String = s"${a.value.toString}"
    override def isReducible(a: Bool): Boolean = false
    override def reduce(a: Bool)(implicit env: Expr.Env): Product = Product(a)
    override def evaluate(a: Bool)(implicit env: Env): Product = Product(a)
  }

  implicit def lessThanConstruct(implicit cs: Construct[Expr]): Construct[LessThan] = new Construct[LessThan] {
    override def syntax(a: LessThan): String = s"${cs.syntax(a.left)} < ${cs.syntax(a.right)}"
    override def isReducible(a: LessThan): Boolean = true
    override def reduce(a: LessThan)(implicit env: Expr.Env): Product = a match {
      case LessThan(left, right) if cs.isReducible(left) => Product(LessThan(cs.reduce(left)(env).expr, right))
      case LessThan(left, right) if cs.isReducible(right) => Product(LessThan(left, cs.reduce(right)(env).expr))
      case LessThan(Num(x), Num(y)) => Product(Bool(x < y))
    }
    override def evaluate(a: LessThan)(implicit env: Env): Product = {
      (cs.evaluate(a.left).expr, cs.evaluate(a.right).expr) match {
        case (Num(x), Num(y)) => Product(Bool(x < y))
        case _ => throw new Exception("evaluate: something went wrong")
      }
    }
  }

  implicit val varConstruct: Construct[Var] = new Construct[Var] {
    override def syntax(a: Var): String = s"${a.name.toString}"
    override def isReducible(a: Var): Boolean = true
    override def reduce(a: Var)(implicit env: Expr.Env): Product = Product(env(a.name))
    override def evaluate(a: Var)(implicit env: Env): Product = Product(env(a.name))
  }

  implicit val doNothingConstruct: Construct[DoNothing.type] = new Construct[DoNothing.type] {
    override def syntax(a: DoNothing.type): String = s"do-nothing"
    override def isReducible(a: DoNothing.type): Boolean = false
    override def reduce(a: DoNothing.type)(implicit env: Expr.Env): Product = Product(a, env)
    override def evaluate(a: DoNothing.type)(implicit env: Env): Product = Product(a, env)
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
    override def evaluate(a: Assign)(implicit env: Env): Product = Product(DoNothing, env + (a.name -> cs.evaluate(a.expr).expr))
  }

  implicit def ifConstruct(implicit cs: Construct[Expr]): Construct[If] = new Construct[If] {
    override def syntax(a: If): String =
      s"if (${cs.syntax(a.condition)}) { ${cs.syntax(a.consequence)} } else { ${cs.syntax(a.alternative)} }"
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
    override def evaluate(a: If)(implicit env: Env): Product = {
      cs.evaluate(a.condition).expr match {
        case Bool(true) => cs.evaluate(a.consequence)
        case Bool(false) => cs.evaluate(a.alternative)
        case _ => throw new Exception("evaluate: something went wrong")
      }
    }
  }

  implicit def seqConstruct(implicit cs: Construct[Expr]): Construct[Seq] = new Construct[Seq] {
    override def syntax(a: Seq): String = s"{ ${cs.syntax(a.first)}; ${cs.syntax(a.second)} }"
    override def isReducible(a: Seq): Boolean = true
    override def reduce(a: Seq)(implicit env: Env): Product = {
      a.first match {
        case _: DoNothing.type => Product(a.second, env)
        case _ => {
          val Product(reducedExpr, reducedEnv) = cs.reduce(a.first)
          Product(Seq(reducedExpr, a.second), reducedEnv)
        }
      }
    }

    override def evaluate(a: Seq)(implicit env: Env): Product =
      Product(DoNothing, cs.evaluate(a.second)(cs.evaluate(a.first).env.getOrElse(Map.empty)).env)
  }

  implicit def whileConstruct(implicit cs: Construct[Expr]): Construct[While] = new Construct[While] {
    override def syntax(a: While): String = s"while (${cs.syntax(a.condition)}) { ${cs.syntax(a.body)} }"
    override def isReducible(a: While): Boolean = true
    override def reduce(a: While)(implicit env: Env): Product = {
      Product(If(a.condition, Seq(a.body, a), DoNothing) , env)
    }
    override def evaluate(a: While)(implicit env: Env): Product = {
      cs.evaluate(a.condition).expr match {
        case Bool(false) => Product(DoNothing, env)
        case Bool(true) => {
          val Product(_, newEnv) = cs.evaluate(a.body)
          evaluate(a)(newEnv.getOrElse(Map.empty))
        }
        case _ => throw new Exception("evaluate: something went wrong")
      }
    }
  }
}