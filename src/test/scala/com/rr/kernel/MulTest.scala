package com.rr.kernel

import com.rr.kernel.ConstructImplicits._
import com.rr.kernel.ConstructSyntax._

class MulTest extends UnitTest("Mul") {

  "Mul" should "convernt in string as" in {
    val mul = Mul(Num(1), Num(2))
    mul.string should be ("Mul(Num(1),Num(2))")
    mul.syntax should be ("1 * 2")
    mul.isReducible should be (true)
    mul.isReducible should not be (false)
  }
}
