package com.github.heyrutvik.simple

import fastparse.all.{P, Parser, _}

object Parser {

  val name: Parser[Unit] = CharIn('a' to 'z').rep(1)
  val variable: Parser[Var] = P(name.!).map(x => Var(Symbol(x)))
  val boolean: Parser[Bool] = P("true".! | "false".!).map(x => Bool(x.toBoolean))
  val number: Parser[Num] = P(CharIn('0' to '9').rep(1).!).map(x => Num(x.toLong))

  val term: Parser[Expr] = boolean | variable | number

  val params: Parser[Expr] = P("(" ~ lessThan ~ ")")
  val factor: Parser[Expr] = term | params

  val mul: Parser[Expr] = P(factor ~ (P(" * ") ~ factor).rep).map {
    case (f, r) => r.foldLeft(f) {
      case (z, b) => Mul(z, b)
    }
  }

  val add: Parser[Expr] = P(mul ~ (P(" + ") ~ mul).rep).map {
    case (f, r) => r.foldLeft(f) {
      case (z, b) => Add(z, b)
    }
  }

  val lessThan: Parser[Expr] = P(add ~ (P(" < ") ~ add).rep).map {
    case (f, r) => r.foldLeft(f) {
      case (z, b) => LessThan(z, b)
    }
  }

  val expression: Parser[Expr] = lessThan

  val assign: Parser[Assign] = P(name.! ~ P(" = ").rep(exactly = 1) ~ expression).map {
    case (name, expr) => Assign(Symbol(name), expr)
  }

  val whileLoop: Parser[While] = P(P("while (").rep(exactly = 1) ~ expression ~ P(") {").rep(exactly = 1) ~ whitespace ~ statement ~ whitespace ~ P("}").rep(exactly = 1)).map {
    case (cond, expr) => While(cond, expr)
  }

  val ifCond: Parser[If] = {
    P(P("if (").rep(exactly = 1) ~ expression ~
      P(") {").rep(exactly = 1) ~ whitespace ~ statement ~ whitespace ~
      P("} else {").rep(exactly = 1) ~ whitespace ~ statement ~ whitespace ~
      P("}").rep(exactly = 1))
      .map {
        case (cond, conse, alter) => If(cond, conse, alter)
      }
  }

  val whitespace = CharsWhileIn(" \r\n")

  val sequence: Parser[Expr] = P(subStatement ~ P(P(";") ~ whitespace ~ subStatement).rep).map {
    case (f, s) => s.foldLeft(f) {
      case (z, b) => Seq(z, b)
    }
  }

  val subStatement: Parser[Expr] = whileLoop | assign | ifCond

  val statement: Parser[Expr] = sequence | subStatement
}
