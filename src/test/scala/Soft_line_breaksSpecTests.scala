package xyz.hyperreal.commonmark

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class Soft_line_breaksSpecTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

  "example 620" in {
    test( "foo\nbaz\n" ) shouldBe "<p>foo\nbaz</p>\n"
  }

  "example 621" in {
    test( "foo \n baz\n" ) shouldBe "<p>foo\nbaz</p>\n"
  }

}
