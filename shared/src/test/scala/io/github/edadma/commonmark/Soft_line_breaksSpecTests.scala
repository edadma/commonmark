package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Soft_line_breaksSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 648" in {
    test( "foo\nbaz\n" ) shouldBe "<p>foo\nbaz</p>\n"
  }

  "example 649" in {
    test( "foo \n baz\n" ) shouldBe "<p>foo\nbaz</p>\n"
  }

}
