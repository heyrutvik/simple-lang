package com.github.heyrutvik.simple

import Expr.Env
import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr, Lazy}

trait Construct[A] {
  def string(a: A): String = a.toString
  def syntax(a: A): String
  def isReducible(a: A): Boolean
  def reduce(a: A)(implicit env: Expr.Env): Product
  def evaluate(a: A)(implicit env: Expr.Env): Product
  def toJs(a: A): String
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
    def toJs(implicit c: Construct[A]): String = c.toJs(value)
  }
}

object ConstructImplicits { self =>

  implicit def genericConstruct[A, R](
    implicit gen: Generic.Aux[A, R], c: Lazy[Construct[R]]): Construct[A] = {
    new Construct[A] {
      override def syntax(a: A): String = c.value.syntax(gen.to(a))
      override def isReducible(a: A): Boolean = c.value.isReducible(gen.to(a))
      override def reduce(a: A)(implicit env: Env): Product = c.value.reduce(gen.to(a))
      override def evaluate(a: A)(implicit env: Env): Product = c.value.evaluate(gen.to(a))
      override def toJs(a: A): String = c.value.toJs(gen.to(a))
    }
  }

  implicit val cnilConstruct: Construct[CNil] = new Construct[CNil] {
    private def inconceivable = throw new Exception("Inconceivable")
    override def syntax(a: CNil): String = inconceivable
    override def isReducible(a: CNil): Boolean = inconceivable
    override def reduce(a: CNil)(implicit env: Env): Product = inconceivable
    override def evaluate(a: CNil)(implicit env: Env): Product = inconceivable
    override def toJs(a: CNil): String = inconceivable
  }

  implicit def coproductConstruct[H, T <: Coproduct](
    implicit hc: Lazy[Construct[H]], tc: Construct[T]): Construct[H :+: T] = {
    new Construct[H :+: T] {
      override def syntax(a: :+:[H, T]): String = a match {
        case Inl(h) => hc.value.syntax(h)
        case Inr(t) => tc.syntax(t)
      }
      override def isReducible(a: :+:[H, T]): Boolean = a match {
        case Inl(h) => hc.value.isReducible(h)
        case Inr(t) => tc.isReducible(t)
      }
      override def reduce(a: :+:[H, T])(implicit env: Env): Product = a match {
        case Inl(h) => hc.value.reduce(h)
        case Inr(t) => tc.reduce(t)
      }
      override def evaluate(a: :+:[H, T])(implicit env: Env): Product = a match {
        case Inl(h) => hc.value.evaluate(h)
        case Inr(t) => tc.evaluate(t)
      }
      override def toJs(a: :+:[H, T]): String = a match {
        case Inl(h) => hc.value.toJs(h)
        case Inr(t) => tc.toJs(t)
      }
    }
  }

  implicit val numberConstruct: Construct[Num] = new Construct[Num] {
    override def syntax(a: Num): String = s"${a.value.toString}"
    override def isReducible(a: Num): Boolean = false
    override def reduce(a: Num)(implicit env: Expr.Env): Product = Product(a)
    override def evaluate(a: Num)(implicit env: Env): Product = Product(a)
    override def toJs(a: Num): String = s"e => ${a.value}"
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
    override def toJs(a: Add): String = s"e => (${cs.toJs(a.left)})(e) + (${cs.toJs(a.right)})(e)"
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
    override def toJs(a: Mul): String = s"e => (${cs.toJs(a.left)})(e) * (${cs.toJs(a.right)})(e)"
  }

  implicit val boolConstruct: Construct[Bool] = new Construct[Bool] {
    override def syntax(a: Bool): String = s"${a.value.toString}"
    override def isReducible(a: Bool): Boolean = false
    override def reduce(a: Bool)(implicit env: Expr.Env): Product = Product(a)
    override def evaluate(a: Bool)(implicit env: Env): Product = Product(a)
    override def toJs(a: Bool): String = s"e => ${a.value}"
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
    override def toJs(a: LessThan): String = s"e => (${cs.toJs(a.left)})(e) < (${cs.toJs(a.right)})(e)"
  }

  implicit val varConstruct: Construct[Var] = new Construct[Var] {
    override def syntax(a: Var): String = s"${a.name.name.toString}"
    override def isReducible(a: Var): Boolean = true
    override def reduce(a: Var)(implicit env: Expr.Env): Product = Product(env(a.name))
    override def evaluate(a: Var)(implicit env: Env): Product = Product(env(a.name))
    override def toJs(a: Var): String = s"e => e.get('${a.name.name}')"
  }

  implicit val doNothingConstruct: Construct[DoNothing.type] = new Construct[DoNothing.type] {
    override def syntax(a: DoNothing.type): String = s"do-nothing"
    override def isReducible(a: DoNothing.type): Boolean = false
    override def reduce(a: DoNothing.type)(implicit env: Expr.Env): Product = Product(a, env)
    override def evaluate(a: DoNothing.type)(implicit env: Env): Product = Product(a, env)
    override def toJs(a: DoNothing.type): String = s"e => e"
  }

  implicit def assignConstruct(implicit cs: Construct[Expr]): Construct[Assign] = new Construct[Assign] {
    override def syntax(a: Assign): String = s"${a.name.name} = ${cs.syntax(a.expr)}"
    override def isReducible(a: Assign): Boolean = true
    override def reduce(a: Assign)(implicit env: Env): Product = {
      if (cs.isReducible(a.expr))
        Product(Assign(a.name, cs.reduce(a.expr)(env).expr), env)
      else
        Product(DoNothing, env + (a.name -> a.expr))
    }
    override def evaluate(a: Assign)(implicit env: Env): Product = Product(DoNothing, env + (a.name -> cs.evaluate(a.expr).expr))
    override def toJs(a: Assign): String = s"e => e.set('${a.name.name}', (${cs.toJs(a.expr)})(e))"
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
    override def toJs(a: If): String =
      s"e => { if ((${cs.toJs(a.condition)})(e)) { return (${cs.toJs(a.consequence)})(e) } else { return (${cs.toJs(a.alternative)})(e)} }"
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
    override def toJs(a: Seq): String = s"e => { return ((${cs.toJs(a.second)}))((${cs.toJs(a.first)})(e)) }"
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
    override def toJs(a: While): String =
      s"e => { while ((${cs.toJs(a.condition)})(e)) { e = (${cs.toJs(a.body)})(e) } return e;}"
  }
}