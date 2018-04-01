package com.rr.kernel

sealed trait Syntax
case class Num(value: Long) extends Syntax
case class Add(left: Syntax, right: Syntax) extends Syntax
case class Mul(left: Syntax, right: Syntax) extends Syntax