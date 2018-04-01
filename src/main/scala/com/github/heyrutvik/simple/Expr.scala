package com.github.heyrutvik.simple

sealed trait Expr
case class Num(value: Long) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Bool(value: Boolean) extends Expr
case class LessThan(left: Expr, right: Expr) extends Expr
case class Var(name: Symbol) extends Expr
case object DoNothing extends Expr
case class Assign(name: Symbol, expr: Expr) extends Expr
case class If(condition: Expr, consequence: Expr, alternative: Expr) extends Expr
case class Seq(first: Expr, second: Expr) extends Expr

object Expr {

  type Env = Map[Symbol, Expr]
}