package com.github.heyrutvik.simple

import ConstructImplicits._
import ConstructSyntax._

class ExpressionTest extends UnitTest("Expression") {

  "Expression" should "follow as" in {
    val environment: Expr.Env = Map.empty
    val expression1 = Add(Mul(Num(1), Num(2)), Mul(Num(3), Num(4)))
    expression1.syntax should be ("1 * 2 + 3 * 4")
    expression1.isReducible should be (true)
    val expression2 = expression1.reduce(environment)
    expression2.syntax should be ("2 + 3 * 4")
    expression2.isReducible should  not be (false)
    val expression3 = expression2.reduce(environment)
    expression3.syntax should be ("2 + 12")
    expression3.isReducible should be (true)
    val expression4 = expression3.reduce(environment)
    expression4.reduce(environment).syntax should be ("14")
    expression4.isReducible should be (false)
  }
}
