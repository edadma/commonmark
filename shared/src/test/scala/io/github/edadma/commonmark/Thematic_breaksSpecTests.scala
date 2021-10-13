package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Thematic_breaksSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 43" in {
    test( "***\n---\n___\n" ) shouldBe "<hr />\n<hr />\n<hr />\n"
  }

  "example 44" in {
    test( "+++\n" ) shouldBe "<p>+++</p>\n"
  }

  "example 45" in {
    test( "===\n" ) shouldBe "<p>===</p>\n"
  }

  "example 46" in {
    test( "--\n**\n__\n" ) shouldBe "<p>--\n**\n__</p>\n"
  }

  "example 47" in {
    test( " ***\n  ***\n   ***\n" ) shouldBe "<hr />\n<hr />\n<hr />\n"
  }

  "example 48" in {
    test( "    ***\n" ) shouldBe "<pre><code>***\n</code></pre>\n"
  }

  "example 49" in {
    test( "Foo\n    ***\n" ) shouldBe "<p>Foo\n***</p>\n"
  }

  "example 50" in {
    test( "_____________________________________\n" ) shouldBe "<hr />\n"
  }

  "example 51" in {
    test( " - - -\n" ) shouldBe "<hr />\n"
  }

  "example 52" in {
    test( " **  * ** * ** * **\n" ) shouldBe "<hr />\n"
  }

  "example 53" in {
    test( "-     -      -      -\n" ) shouldBe "<hr />\n"
  }

  "example 54" in {
    test( "- - - -    \n" ) shouldBe "<hr />\n"
  }

  "example 55" in {
    test( "_ _ _ _ a\n\na------\n\n---a---\n" ) shouldBe "<p>_ _ _ _ a</p>\n<p>a------</p>\n<p>---a---</p>\n"
  }

  "example 56" in {
    test( " *-*\n" ) shouldBe "<p><em>-</em></p>\n"
  }

  "example 57" in {
    test( "- foo\n***\n- bar\n" ) shouldBe "<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>\n"
  }

  "example 58" in {
    test( "Foo\n***\nbar\n" ) shouldBe "<p>Foo</p>\n<hr />\n<p>bar</p>\n"
  }

  "example 59" in {
    test( "Foo\n---\nbar\n" ) shouldBe "<h2>Foo</h2>\n<p>bar</p>\n"
  }

  "example 60" in {
    test( "* Foo\n* * *\n* Bar\n" ) shouldBe "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n<ul>\n<li>Bar</li>\n</ul>\n"
  }

  "example 61" in {
    test( "- Foo\n- * * *\n" ) shouldBe "<ul>\n<li>Foo</li>\n<li>\n<hr />\n</li>\n</ul>\n"
  }

}
