package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class Indented_code_blocksSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 76" in {
    test( "    a simple\n      indented code block\n" ) shouldBe "<pre><code>a simple\n  indented code block\n</code></pre>\n"
  }

  "example 77" in {
    test( "  - foo\n\n    bar\n" ) shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
  }

  "example 78" in {
    test( "1.  foo\n\n    - bar\n" ) shouldBe "<ol>\n<li>\n<p>foo</p>\n<ul>\n<li>bar</li>\n</ul>\n</li>\n</ol>\n"
  }

  "example 79" in {
    test( "    <a/>\n    *hi*\n\n    - one\n" ) shouldBe "<pre><code>&lt;a/&gt;\n*hi*\n\n- one\n</code></pre>\n"
  }

  "example 80" in {
    test( "    chunk1\n\n    chunk2\n  \n \n \n    chunk3\n" ) shouldBe "<pre><code>chunk1\n\nchunk2\n\n\n\nchunk3\n</code></pre>\n"
  }

  "example 81" in {
    test( "    chunk1\n      \n      chunk2\n" ) shouldBe "<pre><code>chunk1\n  \n  chunk2\n</code></pre>\n"
  }

  "example 82" in {
    test( "Foo\n    bar\n\n" ) shouldBe "<p>Foo\nbar</p>\n"
  }

  "example 83" in {
    test( "    foo\nbar\n" ) shouldBe "<pre><code>foo\n</code></pre>\n<p>bar</p>\n"
  }

  "example 84" in {
    test( "# Heading\n    foo\nHeading\n------\n    foo\n----\n" ) shouldBe "<h1>Heading</h1>\n<pre><code>foo\n</code></pre>\n<h2>Heading</h2>\n<pre><code>foo\n</code></pre>\n<hr />\n"
  }

  "example 85" in {
    test( "        foo\n    bar\n" ) shouldBe "<pre><code>    foo\nbar\n</code></pre>\n"
  }

  "example 86" in {
    test( "\n    \n    foo\n    \n\n" ) shouldBe "<pre><code>foo\n</code></pre>\n"
  }

  "example 87" in {
    test( "    foo  \n" ) shouldBe "<pre><code>foo  \n</code></pre>\n"
  }

}
