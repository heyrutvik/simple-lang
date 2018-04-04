package com.github.heyrutvik.simple

import fastparse.all._

object Parser {

  val number = P(CharIn('0' to '9').rep(1).!).map(x => Num(x.toLong))
  val sum = P(number ~ CharIn("+*").! ~ number).map { case (n, op, m) =>
    op match {
      case "+" => Add(n, m)
      case "*" => Mul(n, m)
    }
  }
}
