package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class Textual_contentSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 622" in {
    test( "hello $.;'there\n" ) shouldBe "<p>hello $.;'there</p>\n"
  }

  "example 623" in {
    test( "Foo χρῆν\n" ) shouldBe "<p>Foo χρῆν</p>\n"
  }

  "example 624" in {
    test( "Multiple     spaces\n" ) shouldBe "<p>Multiple     spaces</p>\n"
  }

}
