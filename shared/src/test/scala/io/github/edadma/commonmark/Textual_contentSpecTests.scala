package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Textual_contentSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 650" in {
    test( "hello $.;'there\n" ) shouldBe "<p>hello $.;'there</p>\n"
  }

  "example 651" in {
    test( "Foo χρῆν\n" ) shouldBe "<p>Foo χρῆν</p>\n"
  }

  "example 652" in {
    test( "Multiple     spaces\n" ) shouldBe "<p>Multiple     spaces</p>\n"
  }

}
