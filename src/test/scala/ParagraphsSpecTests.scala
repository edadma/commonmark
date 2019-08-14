package xyz.hyperreal.commonmark

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class ParagraphsSpecTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

  "example 182" in {
    test( "aaa\n\nbbb\n" ) shouldBe "<p>aaa</p>\n<p>bbb</p>\n"
  }

  "example 183" in {
    test( "aaa\nbbb\n\nccc\nddd\n" ) shouldBe "<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n"
  }

  "example 184" in {
    test( "aaa\n\n\nbbb\n" ) shouldBe "<p>aaa</p>\n<p>bbb</p>\n"
  }

  "example 185" in {
    test( "  aaa\n bbb\n" ) shouldBe "<p>aaa\nbbb</p>\n"
  }

  "example 186" in {
    test( "aaa\n             bbb\n                                       ccc\n" ) shouldBe "<p>aaa\nbbb\nccc</p>\n"
  }

  "example 187" in {
    test( "   aaa\nbbb\n" ) shouldBe "<p>aaa\nbbb</p>\n"
  }

  "example 188" in {
    test( "    aaa\nbbb\n" ) shouldBe "<pre><code>aaa\n</code></pre>\n<p>bbb</p>\n"
  }

  "example 189" in {
    test( "aaa     \nbbb     \n" ) shouldBe "<p>aaa<br />\nbbb</p>\n"
  }

}
