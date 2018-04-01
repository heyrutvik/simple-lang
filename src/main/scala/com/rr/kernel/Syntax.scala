package com.rr.kernel

sealed trait Syntax
case class Num(value: Long) extends Syntax
case class Add[A, B](left: A, right: B) extends Syntax
case class Mul[A, B](left: A, right: B) extends Syntax