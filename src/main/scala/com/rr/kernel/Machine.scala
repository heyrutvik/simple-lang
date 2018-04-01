package com.rr.kernel

import cats.data.State
import ConstructImplicits._
import ConstructSyntax._

case class Machine(expr: Expr) {
  def syntax = s"<<${expr.syntax}>>"
}

object Machine extends App {

  type MachineState[A] = State[Machine, A]

  def interpreter(expr: Expr) = {

    def end: MachineState[Boolean] = State.inspect(_ => false)

    def execute: MachineState[Boolean] = State { machine =>
      (Machine(machine.expr.reduce), machine.expr.isReducible)
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

    exec.runA(Machine(expr)).value.expr.syntax
  }

  println(interpreter(Add(Mul(Num(1), Num(2)), Mul(Num(3), Num(4)))))
}