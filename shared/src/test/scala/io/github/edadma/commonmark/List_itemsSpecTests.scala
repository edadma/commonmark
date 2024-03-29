package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class List_itemsSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 253" in {
    test( "A paragraph\nwith two lines.\n\n    indented code\n\n> A block quote.\n" ) shouldBe "<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n"
  }

  "example 254" in {
    test( "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote.\n" ) shouldBe "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
  }

  "example 255" in {
    test( "- one\n\n two\n" ) shouldBe "<ul>\n<li>one</li>\n</ul>\n<p>two</p>\n"
  }

  "example 256" in {
    test( "- one\n\n  two\n" ) shouldBe "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"
  }

  "example 257" in {
    test( " -    one\n\n     two\n" ) shouldBe "<ul>\n<li>one</li>\n</ul>\n<pre><code> two\n</code></pre>\n"
  }

  "example 258" in {
    test( " -    one\n\n      two\n" ) shouldBe "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"
  }

  "example 259" in {
    test( "   > > 1.  one\n>>\n>>     two\n" ) shouldBe "<blockquote>\n<blockquote>\n<ol>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ol>\n</blockquote>\n</blockquote>\n"
  }

  "example 260" in {
    test( ">>- one\n>>\n  >  > two\n" ) shouldBe "<blockquote>\n<blockquote>\n<ul>\n<li>one</li>\n</ul>\n<p>two</p>\n</blockquote>\n</blockquote>\n"
  }

  "example 261" in {
    test( "-one\n\n2.two\n" ) shouldBe "<p>-one</p>\n<p>2.two</p>\n"
  }

  "example 262" in {
    test( "- foo\n\n\n  bar\n" ) shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
  }

  "example 263" in {
    test( "1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam\n" ) shouldBe "<ol>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n<blockquote>\n<p>bam</p>\n</blockquote>\n</li>\n</ol>\n"
  }

  "example 264" in {
    test( "- Foo\n\n      bar\n\n\n      baz\n" ) shouldBe "<ul>\n<li>\n<p>Foo</p>\n<pre><code>bar\n\n\nbaz\n</code></pre>\n</li>\n</ul>\n"
  }

  "example 265" in {
    test( "123456789. ok\n" ) shouldBe "<ol start=\"123456789\">\n<li>ok</li>\n</ol>\n"
  }

  "example 266" in {
    test( "1234567890. not ok\n" ) shouldBe "<p>1234567890. not ok</p>\n"
  }

  "example 267" in {
    test( "0. ok\n" ) shouldBe "<ol start=\"0\">\n<li>ok</li>\n</ol>\n"
  }

  "example 268" in {
    test( "003. ok\n" ) shouldBe "<ol start=\"3\">\n<li>ok</li>\n</ol>\n"
  }

  "example 269" in {
    test( "-1. not ok\n" ) shouldBe "<p>-1. not ok</p>\n"
  }

  "example 270" in {
    test( "- foo\n\n      bar\n" ) shouldBe "<ul>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ul>\n"
  }

  "example 271" in {
    test( "  10.  foo\n\n           bar\n" ) shouldBe "<ol start=\"10\">\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ol>\n"
  }

  "example 272" in {
    test( "    indented code\n\nparagraph\n\n    more code\n" ) shouldBe "<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n"
  }

  "example 273" in {
    test( "1.     indented code\n\n   paragraph\n\n       more code\n" ) shouldBe "<ol>\n<li>\n<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>\n"
  }

  "example 274" in {
    test( "1.      indented code\n\n   paragraph\n\n       more code\n" ) shouldBe "<ol>\n<li>\n<pre><code> indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>\n"
  }

  "example 275" in {
    test( "   foo\n\nbar\n" ) shouldBe "<p>foo</p>\n<p>bar</p>\n"
  }

  "example 276" in {
    test( "-    foo\n\n  bar\n" ) shouldBe "<ul>\n<li>foo</li>\n</ul>\n<p>bar</p>\n"
  }

  "example 277" in {
    test( "-  foo\n\n   bar\n" ) shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
  }

  "example 278" in {
    test( "-\n  foo\n-\n  ```\n  bar\n  ```\n-\n      baz\n" ) shouldBe "<ul>\n<li>foo</li>\n<li>\n<pre><code>bar\n</code></pre>\n</li>\n<li>\n<pre><code>baz\n</code></pre>\n</li>\n</ul>\n"
  }

  "example 279" in {
    test( "-   \n  foo\n" ) shouldBe "<ul>\n<li>foo</li>\n</ul>\n"
  }

  "example 280" in {
    test( "-\n\n  foo\n" ) shouldBe "<ul>\n<li></li>\n</ul>\n<p>foo</p>\n"
  }

  "example 281" in {
    test( "- foo\n-\n- bar\n" ) shouldBe "<ul>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ul>\n"
  }

  "example 282" in {
    test( "- foo\n-   \n- bar\n" ) shouldBe "<ul>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ul>\n"
  }

  "example 283" in {
    test( "1. foo\n2.\n3. bar\n" ) shouldBe "<ol>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ol>\n"
  }

  "example 284" in {
    test( "*\n" ) shouldBe "<ul>\n<li></li>\n</ul>\n"
  }

  "example 285" in {
    test( "foo\n*\n\nfoo\n1.\n" ) shouldBe "<p>foo\n*</p>\n<p>foo\n1.</p>\n"
  }

  "example 286" in {
    test( " 1.  A paragraph\n     with two lines.\n\n         indented code\n\n     > A block quote.\n" ) shouldBe "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
  }

  "example 287" in {
    test( "  1.  A paragraph\n      with two lines.\n\n          indented code\n\n      > A block quote.\n" ) shouldBe "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
  }

  "example 288" in {
    test( "   1.  A paragraph\n       with two lines.\n\n           indented code\n\n       > A block quote.\n" ) shouldBe "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
  }

  "example 289" in {
    test( "    1.  A paragraph\n        with two lines.\n\n            indented code\n\n        > A block quote.\n" ) shouldBe "<pre><code>1.  A paragraph\n    with two lines.\n\n        indented code\n\n    &gt; A block quote.\n</code></pre>\n"
  }

  "example 290" in {
    test( "  1.  A paragraph\nwith two lines.\n\n          indented code\n\n      > A block quote.\n" ) shouldBe "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
  }

  "example 291" in {
    test( "  1.  A paragraph\n    with two lines.\n" ) shouldBe "<ol>\n<li>A paragraph\nwith two lines.</li>\n</ol>\n"
  }

  "example 292" in {
    test( "> 1. > Blockquote\ncontinued here.\n" ) shouldBe "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>\n"
  }

  "example 293" in {
    test( "> 1. > Blockquote\n> continued here.\n" ) shouldBe "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>\n"
  }

  "example 294" in {
    test( "- foo\n  - bar\n    - baz\n      - boo\n" ) shouldBe "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>baz\n<ul>\n<li>boo</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
  }

  "example 295" in {
    test( "- foo\n - bar\n  - baz\n   - boo\n" ) shouldBe "<ul>\n<li>foo</li>\n<li>bar</li>\n<li>baz</li>\n<li>boo</li>\n</ul>\n"
  }

  "example 296" in {
    test( "10) foo\n    - bar\n" ) shouldBe "<ol start=\"10\">\n<li>foo\n<ul>\n<li>bar</li>\n</ul>\n</li>\n</ol>\n"
  }

  "example 297" in {
    test( "10) foo\n   - bar\n" ) shouldBe "<ol start=\"10\">\n<li>foo</li>\n</ol>\n<ul>\n<li>bar</li>\n</ul>\n"
  }

  "example 298" in {
    test( "- - foo\n" ) shouldBe "<ul>\n<li>\n<ul>\n<li>foo</li>\n</ul>\n</li>\n</ul>\n"
  }

  "example 299" in {
    test( "1. - 2. foo\n" ) shouldBe "<ol>\n<li>\n<ul>\n<li>\n<ol start=\"2\">\n<li>foo</li>\n</ol>\n</li>\n</ul>\n</li>\n</ol>\n"
  }

  "example 300" in {
    test( "- # Foo\n- Bar\n  ---\n  baz\n" ) shouldBe "<ul>\n<li>\n<h1>Foo</h1>\n</li>\n<li>\n<h2>Bar</h2>\nbaz</li>\n</ul>\n"
  }

}
