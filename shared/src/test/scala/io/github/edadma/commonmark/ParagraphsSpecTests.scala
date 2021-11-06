package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParagraphsSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 219" in {
    test( "aaa\n\nbbb\n" ) shouldBe "<p>aaa</p>\n<p>bbb</p>\n"
  }

  "example 220" in {
    test( "aaa\nbbb\n\nccc\nddd\n" ) shouldBe "<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n"
  }

  "example 221" in {
    test( "aaa\n\n\nbbb\n" ) shouldBe "<p>aaa</p>\n<p>bbb</p>\n"
  }

  "example 222" in {
    test( "  aaa\n bbb\n" ) shouldBe "<p>aaa\nbbb</p>\n"
  }

  "example 223" in {
    test( "aaa\n             bbb\n                                       ccc\n" ) shouldBe "<p>aaa\nbbb\nccc</p>\n"
  }

  "example 224" in {
    test( "   aaa\nbbb\n" ) shouldBe "<p>aaa\nbbb</p>\n"
  }

  "example 225" in {
    test( "    aaa\nbbb\n" ) shouldBe "<pre><code>aaa\n</code></pre>\n<p>bbb</p>\n"
  }

  "example 226" in {
    test( "aaa     \nbbb     \n" ) shouldBe "<p>aaa<br />\nbbb</p>\n"
  }

}
