package com.github.heyrutvik.simple

import ConstructImplicits._
import ConstructSyntax._

class NumTest extends UnitTest("Num") {

  "Number" should "convernt in string as" in {
    val ten = Num(10)
    ten.string should be ("Num(10)")
    ten.syntax should be ("10")
    ten.isReducible should be (false)
  }
}
