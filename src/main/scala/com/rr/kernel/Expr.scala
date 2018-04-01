package com.rr.kernel

sealed trait Expr
case class Num(value: Long) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr