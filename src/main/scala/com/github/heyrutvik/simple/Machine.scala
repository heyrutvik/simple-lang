package com.github.heyrutvik.simple

import cats.data.State
import ConstructImplicits._
import ConstructSyntax._

case class Machine(expr: Expr, env: Expr.Env) {
  def print = println(s"\nexpr: ${expr.syntax}\nenv: $env\n")
}

object Machine {

  type MachineState[A] = State[Machine, A]

  def interpreter(expr: Expr, env: Expr.Env) = {

    def end: MachineState[Boolean] = State.inspect(_ => false)

    def execute: MachineState[Boolean] = State { machine =>
//      machine.print
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
}