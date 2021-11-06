package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Blank_linesSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 227" in {
    test( "  \n\naaa\n  \n\n# aaa\n\n  \n" ) shouldBe "<p>aaa</p>\n<h1>aaa</h1>\n"
  }

}
