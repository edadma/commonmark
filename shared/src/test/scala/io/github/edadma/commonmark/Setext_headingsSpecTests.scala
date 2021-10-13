package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Setext_headingsSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 80" in {
    test( "Foo *bar*\n=========\n\nFoo *bar*\n---------\n" ) shouldBe "<h1>Foo <em>bar</em></h1>\n<h2>Foo <em>bar</em></h2>\n"
  }

  "example 81" in {
    test( "Foo *bar\nbaz*\n====\n" ) shouldBe "<h1>Foo <em>bar\nbaz</em></h1>\n"
  }

  "example 82" in {
    test( "  Foo *bar\nbaz*\t\n====\n" ) shouldBe "<h1>Foo <em>bar\nbaz</em></h1>\n"
  }

  "example 83" in {
    test( "Foo\n-------------------------\n\nFoo\n=\n" ) shouldBe "<h2>Foo</h2>\n<h1>Foo</h1>\n"
  }

  "example 84" in {
    test( "   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ===\n" ) shouldBe "<h2>Foo</h2>\n<h2>Foo</h2>\n<h1>Foo</h1>\n"
  }

  "example 85" in {
    test( "    Foo\n    ---\n\n    Foo\n---\n" ) shouldBe "<pre><code>Foo\n---\n\nFoo\n</code></pre>\n<hr />\n"
  }

  "example 86" in {
    test( "Foo\n   ----      \n" ) shouldBe "<h2>Foo</h2>\n"
  }

  "example 87" in {
    test( "Foo\n    ---\n" ) shouldBe "<p>Foo\n---</p>\n"
  }

  "example 88" in {
    test( "Foo\n= =\n\nFoo\n--- -\n" ) shouldBe "<p>Foo\n= =</p>\n<p>Foo</p>\n<hr />\n"
  }

  "example 89" in {
    test( "Foo  \n-----\n" ) shouldBe "<h2>Foo</h2>\n"
  }

  "example 90" in {
    test( "Foo\\\n----\n" ) shouldBe "<h2>Foo\\</h2>\n"
  }

  "example 91" in {
    test( "`Foo\n----\n`\n\n<a title=\"a lot\n---\nof dashes\"/>\n" ) shouldBe "<h2>`Foo</h2>\n<p>`</p>\n<h2>&lt;a title=&quot;a lot</h2>\n<p>of dashes&quot;/&gt;</p>\n"
  }

  "example 92" in {
    test( "> Foo\n---\n" ) shouldBe "<blockquote>\n<p>Foo</p>\n</blockquote>\n<hr />\n"
  }

  "example 93" in {
    test( "> foo\nbar\n===\n" ) shouldBe "<blockquote>\n<p>foo\nbar\n===</p>\n</blockquote>\n"
  }

  "example 94" in {
    test( "- Foo\n---\n" ) shouldBe "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n"
  }

  "example 95" in {
    test( "Foo\nBar\n---\n" ) shouldBe "<h2>Foo\nBar</h2>\n"
  }

  "example 96" in {
    test( "---\nFoo\n---\nBar\n---\nBaz\n" ) shouldBe "<hr />\n<h2>Foo</h2>\n<h2>Bar</h2>\n<p>Baz</p>\n"
  }

  "example 97" in {
    test( "\n====\n" ) shouldBe "<p>====</p>\n"
  }

  "example 98" in {
    test( "---\n---\n" ) shouldBe "<hr />\n<hr />\n"
  }

  "example 99" in {
    test( "- foo\n-----\n" ) shouldBe "<ul>\n<li>foo</li>\n</ul>\n<hr />\n"
  }

  "example 100" in {
    test( "    foo\n---\n" ) shouldBe "<pre><code>foo\n</code></pre>\n<hr />\n"
  }

  "example 101" in {
    test( "> foo\n-----\n" ) shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr />\n"
  }

  "example 102" in {
    test( "\\> foo\n------\n" ) shouldBe "<h2>&gt; foo</h2>\n"
  }

  "example 103" in {
    test( "Foo\n\nbar\n---\nbaz\n" ) shouldBe "<p>Foo</p>\n<h2>bar</h2>\n<p>baz</p>\n"
  }

  "example 104" in {
    test( "Foo\nbar\n\n---\n\nbaz\n" ) shouldBe "<p>Foo\nbar</p>\n<hr />\n<p>baz</p>\n"
  }

  "example 105" in {
    test( "Foo\nbar\n* * *\nbaz\n" ) shouldBe "<p>Foo\nbar</p>\n<hr />\n<p>baz</p>\n"
  }

  "example 106" in {
    test( "Foo\nbar\n\\---\nbaz\n" ) shouldBe "<p>Foo\nbar\n---\nbaz</p>\n"
  }

}
