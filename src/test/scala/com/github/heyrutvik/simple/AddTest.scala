package com.github.heyrutvik.simple

import ConstructImplicits._
import ConstructSyntax._

class AddTest extends UnitTest("Add") {

  "Add" should "convernt in string as" in {
    val add = Add(Num(1), Num(2))
    add.string should be ("Add(Num(1),Num(2))")
    add.syntax should be ("1 + 2")
    add.isReducible should be (true)
    add.isReducible should not be (false)
  }
}
