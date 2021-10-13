package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ListsSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 301" in {
    test( "- foo\n- bar\n+ baz\n" ) shouldBe "<ul>\n<li>foo</li>\n<li>bar</li>\n</ul>\n<ul>\n<li>baz</li>\n</ul>\n"
  }

  "example 302" in {
    test( "1. foo\n2. bar\n3) baz\n" ) shouldBe "<ol>\n<li>foo</li>\n<li>bar</li>\n</ol>\n<ol start=\"3\">\n<li>baz</li>\n</ol>\n"
  }

  "example 303" in {
    test( "Foo\n- bar\n- baz\n" ) shouldBe "<p>Foo</p>\n<ul>\n<li>bar</li>\n<li>baz</li>\n</ul>\n"
  }

  "example 304" in {
    test( "The number of windows in my house is\n14.  The number of doors is 6.\n" ) shouldBe "<p>The number of windows in my house is\n14.  The number of doors is 6.</p>\n"
  }

  "example 305" in {
    test( "The number of windows in my house is\n1.  The number of doors is 6.\n" ) shouldBe "<p>The number of windows in my house is</p>\n<ol>\n<li>The number of doors is 6.</li>\n</ol>\n"
  }

  "example 306" in {
    test( "- foo\n\n- bar\n\n\n- baz\n" ) shouldBe "<ul>\n<li>\n<p>foo</p>\n</li>\n<li>\n<p>bar</p>\n</li>\n<li>\n<p>baz</p>\n</li>\n</ul>\n"
  }

  "example 307" in {
    test( "- foo\n  - bar\n    - baz\n\n\n      bim\n" ) shouldBe "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>\n<p>baz</p>\n<p>bim</p>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
  }

  "example 308" in {
    test( "- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim\n" ) shouldBe "<ul>\n<li>foo</li>\n<li>bar</li>\n</ul>\n<!-- -->\n<ul>\n<li>baz</li>\n<li>bim</li>\n</ul>\n"
  }

  "example 309" in {
    test( "-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code\n" ) shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>notcode</p>\n</li>\n<li>\n<p>foo</p>\n</li>\n</ul>\n<!-- -->\n<pre><code>code\n</code></pre>\n"
  }

  "example 310" in {
    test( "- a\n - b\n  - c\n   - d\n  - e\n - f\n- g\n" ) shouldBe "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d</li>\n<li>e</li>\n<li>f</li>\n<li>g</li>\n</ul>\n"
  }

  "example 311" in {
    test( "1. a\n\n  2. b\n\n   3. c\n" ) shouldBe "<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ol>\n"
  }

  "example 312" in {
    test( "- a\n - b\n  - c\n   - d\n    - e\n" ) shouldBe "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d\n- e</li>\n</ul>\n"
  }

  "example 313" in {
    test( "1. a\n\n  2. b\n\n    3. c\n" ) shouldBe "<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n</ol>\n<pre><code>3. c\n</code></pre>\n"
  }

  "example 314" in {
    test( "- a\n- b\n\n- c\n" ) shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
  }

  "example 315" in {
    test( "* a\n*\n\n* c\n" ) shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li></li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
  }

  "example 316" in {
    test( "- a\n- b\n\n  c\n- d\n" ) shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
  }

  "example 317" in {
    test( "- a\n- b\n\n  [ref]: /url\n- d\n" ) shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
  }

  "example 318" in {
    test( "- a\n- ```\n  b\n\n\n  ```\n- c\n" ) shouldBe "<ul>\n<li>a</li>\n<li>\n<pre><code>b\n\n\n</code></pre>\n</li>\n<li>c</li>\n</ul>\n"
  }

  "example 319" in {
    test( "- a\n  - b\n\n    c\n- d\n" ) shouldBe "<ul>\n<li>a\n<ul>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n</ul>\n</li>\n<li>d</li>\n</ul>\n"
  }

  "example 320" in {
    test( "* a\n  > b\n  >\n* c\n" ) shouldBe "<ul>\n<li>a\n<blockquote>\n<p>b</p>\n</blockquote>\n</li>\n<li>c</li>\n</ul>\n"
  }

  "example 321" in {
    test( "- a\n  > b\n  ```\n  c\n  ```\n- d\n" ) shouldBe "<ul>\n<li>a\n<blockquote>\n<p>b</p>\n</blockquote>\n<pre><code>c\n</code></pre>\n</li>\n<li>d</li>\n</ul>\n"
  }

  "example 322" in {
    test( "- a\n" ) shouldBe "<ul>\n<li>a</li>\n</ul>\n"
  }

  "example 323" in {
    test( "- a\n  - b\n" ) shouldBe "<ul>\n<li>a\n<ul>\n<li>b</li>\n</ul>\n</li>\n</ul>\n"
  }

  "example 324" in {
    test( "1. ```\n   foo\n   ```\n\n   bar\n" ) shouldBe "<ol>\n<li>\n<pre><code>foo\n</code></pre>\n<p>bar</p>\n</li>\n</ol>\n"
  }

  "example 325" in {
    test( "* foo\n  * bar\n\n  baz\n" ) shouldBe "<ul>\n<li>\n<p>foo</p>\n<ul>\n<li>bar</li>\n</ul>\n<p>baz</p>\n</li>\n</ul>\n"
  }

  "example 326" in {
    test( "- a\n  - b\n  - c\n\n- d\n  - e\n  - f\n" ) shouldBe "<ul>\n<li>\n<p>a</p>\n<ul>\n<li>b</li>\n<li>c</li>\n</ul>\n</li>\n<li>\n<p>d</p>\n<ul>\n<li>e</li>\n<li>f</li>\n</ul>\n</li>\n</ul>\n"
  }

}
