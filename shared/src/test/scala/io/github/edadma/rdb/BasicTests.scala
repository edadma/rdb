package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BasicTests extends AnyFreeSpec with Matchers {

  "auto uuid" in {
    List(1, 2, 3) mkString "\n" shouldBe
      """
        |1
        |2
        |3
        """.trim.stripMargin
  }

}
