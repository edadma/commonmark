package io.github.edadma.commonmark

import org.scalatest._
import prop.PropertyChecks

class Thematic_breaksSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 13" in {
    test("***\n---\n___\n") shouldBe "<hr />\n<hr />\n<hr />\n"
  }

  "example 14" in {
    test("+++\n") shouldBe "<p>+++</p>\n"
  }

  "example 15" in {
    test("===\n") shouldBe "<p>===</p>\n"
  }

  "example 16" in {
    test("--\n**\n__\n") shouldBe "<p>--\n**\n__</p>\n"
  }

  "example 17" in {
    test(" ***\n  ***\n   ***\n") shouldBe "<hr />\n<hr />\n<hr />\n"
  }

  "example 18" in {
    test("    ***\n") shouldBe "<pre><code>***\n</code></pre>\n"
  }

  "example 19" in {
    test("Foo\n    ***\n") shouldBe "<p>Foo\n***</p>\n"
  }

  "example 20" in {
    test("_____________________________________\n") shouldBe "<hr />\n"
  }

  "example 21" in {
    test(" - - -\n") shouldBe "<hr />\n"
  }

  "example 22" in {
    test(" **  * ** * ** * **\n") shouldBe "<hr />\n"
  }

  "example 23" in {
    test("-     -      -      -\n") shouldBe "<hr />\n"
  }

  "example 24" in {
    test("- - - -    \n") shouldBe "<hr />\n"
  }

  "example 25" in {
    test("_ _ _ _ a\n\na------\n\n---a---\n") shouldBe "<p>_ _ _ _ a</p>\n<p>a------</p>\n<p>---a---</p>\n"
  }

  //  "example 26" in {
  //    test( " *-*\n" ) shouldBe "<p><em>-</em></p>\n"
  //  }

  "example 27" in {
    test("- foo\n***\n- bar\n") shouldBe "<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>\n"
  }

  "example 28" in {
    test("Foo\n***\nbar\n") shouldBe "<p>Foo</p>\n<hr />\n<p>bar</p>\n"
  }

  "example 29" in {
    test("Foo\n---\nbar\n") shouldBe "<h2>Foo</h2>\n<p>bar</p>\n"
  }

  "example 30" in {
    test("* Foo\n* * *\n* Bar\n") shouldBe "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n<ul>\n<li>Bar</li>\n</ul>\n"
  }

  "example 31" in {
    test("- Foo\n- * * *\n") shouldBe "<ul>\n<li>Foo</li>\n<li>\n<hr />\n</li>\n</ul>\n"
  }

}
