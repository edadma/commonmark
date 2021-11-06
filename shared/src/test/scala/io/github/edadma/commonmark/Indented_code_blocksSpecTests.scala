package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Indented_code_blocksSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 107" in {
    test( "    a simple\n      indented code block\n" ) shouldBe "<pre><code>a simple\n  indented code block\n</code></pre>\n"
  }

  "example 108" in {
    test( "  - foo\n\n    bar\n" ) shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
  }

  "example 109" in {
    test( "1.  foo\n\n    - bar\n" ) shouldBe "<ol>\n<li>\n<p>foo</p>\n<ul>\n<li>bar</li>\n</ul>\n</li>\n</ol>\n"
  }

  "example 110" in {
    test( "    <a/>\n    *hi*\n\n    - one\n" ) shouldBe "<pre><code>&lt;a/&gt;\n*hi*\n\n- one\n</code></pre>\n"
  }

  "example 111" in {
    test( "    chunk1\n\n    chunk2\n  \n \n \n    chunk3\n" ) shouldBe "<pre><code>chunk1\n\nchunk2\n\n\n\nchunk3\n</code></pre>\n"
  }

  "example 112" in {
    test( "    chunk1\n      \n      chunk2\n" ) shouldBe "<pre><code>chunk1\n  \n  chunk2\n</code></pre>\n"
  }

  "example 113" in {
    test( "Foo\n    bar\n\n" ) shouldBe "<p>Foo\nbar</p>\n"
  }

  "example 114" in {
    test( "    foo\nbar\n" ) shouldBe "<pre><code>foo\n</code></pre>\n<p>bar</p>\n"
  }

  "example 115" in {
    test( "# Heading\n    foo\nHeading\n------\n    foo\n----\n" ) shouldBe "<h1>Heading</h1>\n<pre><code>foo\n</code></pre>\n<h2>Heading</h2>\n<pre><code>foo\n</code></pre>\n<hr />\n"
  }

  "example 116" in {
    test( "        foo\n    bar\n" ) shouldBe "<pre><code>    foo\nbar\n</code></pre>\n"
  }

  "example 117" in {
    test( "\n    \n    foo\n    \n\n" ) shouldBe "<pre><code>foo\n</code></pre>\n"
  }

  "example 118" in {
    test( "    foo  \n" ) shouldBe "<pre><code>foo  \n</code></pre>\n"
  }

}
