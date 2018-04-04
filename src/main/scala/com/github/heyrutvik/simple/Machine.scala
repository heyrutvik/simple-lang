package com.github.heyrutvik.simple

import cats.data.State
import ConstructImplicits._
import ConstructSyntax._

case class Machine(expr: Expr, env: Expr.Env) {
  def print = println(s"\nexpression: ${expr.syntax}\nenvironment: ${env.mkString("[", ",", "]")}\n")
}

object Machine {

  type MachineState[A] = State[Machine, A]

  // Operational Semantics
  def interpreter(expr: Expr, env: Expr.Env) = {

    def end: MachineState[Boolean] = State.inspect(_ => false)

    def execute: MachineState[Boolean] = State { machine =>
      machine.print
      val Product(newExpr, newEnv) = machine.expr.reduce(machine.env) // TODO: tricky env vs machine.env
      (Machine(newExpr, newEnv.getOrElse(Map.empty)), newExpr.isReducible)
    }

    def loop(m: MachineState[Boolean]): MachineState[Boolean] = {
      m flatMap {
        case true => loop(m)
        case false => end
      }
    }

    val exec = for {
      _ <- loop(execute)
      m <- State.get
    } yield (m)

    exec.runA(Machine(expr, env)).value
  }

  // Denotational Semantics
  def compileToJs[A](expr: Expr, env: Expr.Env) = {
    s"(${expr.toJs})(new Map(${envPrint(env)}))"
  }

  def envPrint(env: Expr.Env): String = {
    env.map {
      case (sym, expr) => s"['${sym.name}', ${expr.syntax}]"
    }.mkString("[", ",", "]")
  }

  Machine.compileToJs(If(Var('a), Assign('x, Num(10)), While(LessThan(Var('x), Num(5)), Assign('x, Mul(Var('x), Num(3))))), Map('a -> Bool(false), 'x -> Num(1)))
}