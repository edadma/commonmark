package xyz.hyperreal.commonmark

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class Blank_linesSpecTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

  "example 190" in {
    test( "  \n\naaa\n  \n\n# aaa\n\n  \n" ) shouldBe "<p>aaa</p>\n<h1>aaa</h1>\n"
  }

}
