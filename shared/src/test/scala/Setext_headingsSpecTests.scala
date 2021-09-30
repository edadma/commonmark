package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class Setext_headingsSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

//  "example 50" in {
//    test( "Foo *bar*\n=========\n\nFoo *bar*\n---------\n" ) shouldBe "<h1>Foo <em>bar</em></h1>\n<h2>Foo <em>bar</em></h2>\n"
//  }

//  "example 51" in {
//    test( "Foo *bar\nbaz*\n====\n" ) shouldBe "<h1>Foo <em>bar\nbaz</em></h1>\n"
//  }

  "example 52" in {
    test( "Foo\n-------------------------\n\nFoo\n=\n" ) shouldBe "<h2>Foo</h2>\n<h1>Foo</h1>\n"
  }

  "example 53" in {
    test( "   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ===\n" ) shouldBe "<h2>Foo</h2>\n<h2>Foo</h2>\n<h1>Foo</h1>\n"
  }

  "example 54" in {
    test( "    Foo\n    ---\n\n    Foo\n---\n" ) shouldBe "<pre><code>Foo\n---\n\nFoo\n</code></pre>\n<hr />\n"
  }

  "example 55" in {
    test( "Foo\n   ----      \n" ) shouldBe "<h2>Foo</h2>\n"
  }

  "example 56" in {
    test( "Foo\n    ---\n" ) shouldBe "<p>Foo\n---</p>\n"
  }

  "example 57" in {
    test( "Foo\n= =\n\nFoo\n--- -\n" ) shouldBe "<p>Foo\n= =</p>\n<p>Foo</p>\n<hr />\n"
  }

  "example 58" in {
    test( "Foo  \n-----\n" ) shouldBe "<h2>Foo</h2>\n"
  }

  "example 59" in {
    test( "Foo\\\n----\n" ) shouldBe "<h2>Foo\\</h2>\n"
  }

  "example 60" in {
    test( "`Foo\n----\n`\n\n<a title=\"a lot\n---\nof dashes\"/>\n" ) shouldBe "<h2>`Foo</h2>\n<p>`</p>\n<h2>&lt;a title=&quot;a lot</h2>\n<p>of dashes&quot;/&gt;</p>\n"
  }

  "example 61" in {
    test( "> Foo\n---\n" ) shouldBe "<blockquote>\n<p>Foo</p>\n</blockquote>\n<hr />\n"
  }

  "example 62" in {
    test( "> foo\nbar\n===\n" ) shouldBe "<blockquote>\n<p>foo\nbar\n===</p>\n</blockquote>\n"
  }

  "example 63" in {
    test( "- Foo\n---\n" ) shouldBe "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n"
  }

  "example 64" in {
    test( "Foo\nBar\n---\n" ) shouldBe "<h2>Foo\nBar</h2>\n"
  }

  "example 65" in {
    test( "---\nFoo\n---\nBar\n---\nBaz\n" ) shouldBe "<hr />\n<h2>Foo</h2>\n<h2>Bar</h2>\n<p>Baz</p>\n"
  }

  "example 66" in {
    test( "\n====\n" ) shouldBe "<p>====</p>\n"
  }

  "example 67" in {
    test( "---\n---\n" ) shouldBe "<hr />\n<hr />\n"
  }

  "example 68" in {
    test( "- foo\n-----\n" ) shouldBe "<ul>\n<li>foo</li>\n</ul>\n<hr />\n"
  }

  "example 69" in {
    test( "    foo\n---\n" ) shouldBe "<pre><code>foo\n</code></pre>\n<hr />\n"
  }

  "example 70" in {
    test( "> foo\n-----\n" ) shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr />\n"
  }

  "example 71" in {
    test( "\\> foo\n------\n" ) shouldBe "<h2>&gt; foo</h2>\n"
  }

  "example 72" in {
    test( "Foo\n\nbar\n---\nbaz\n" ) shouldBe "<p>Foo</p>\n<h2>bar</h2>\n<p>baz</p>\n"
  }

  "example 73" in {
    test( "Foo\nbar\n\n---\n\nbaz\n" ) shouldBe "<p>Foo\nbar</p>\n<hr />\n<p>baz</p>\n"
  }

  "example 74" in {
    test( "Foo\nbar\n* * *\nbaz\n" ) shouldBe "<p>Foo\nbar</p>\n<hr />\n<p>baz</p>\n"
  }

  "example 75" in {
    test( "Foo\nbar\n\\---\nbaz\n" ) shouldBe "<p>Foo\nbar\n---\nbaz</p>\n"
  }

}
