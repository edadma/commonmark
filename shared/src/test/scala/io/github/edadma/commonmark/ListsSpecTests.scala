package io.github.edadma.commonmark

import org.scalatest._
import prop.PropertyChecks

class ListsSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 264" in {
    test("- foo\n- bar\n+ baz\n") shouldBe "<ul>\n<li>foo</li>\n<li>bar</li>\n</ul>\n<ul>\n<li>baz</li>\n</ul>\n"
  }

  "example 265" in {
    test("1. foo\n2. bar\n3) baz\n") shouldBe "<ol>\n<li>foo</li>\n<li>bar</li>\n</ol>\n<ol start=\"3\">\n<li>baz</li>\n</ol>\n"
  }

  "example 266" in {
    test("Foo\n- bar\n- baz\n") shouldBe "<p>Foo</p>\n<ul>\n<li>bar</li>\n<li>baz</li>\n</ul>\n"
  }

  "example 267" in {
    test("The number of windows in my house is\n14.  The number of doors is 6.\n") shouldBe "<p>The number of windows in my house is\n14.  The number of doors is 6.</p>\n"
  }

  "example 268" in {
    test("The number of windows in my house is\n1.  The number of doors is 6.\n") shouldBe "<p>The number of windows in my house is</p>\n<ol>\n<li>The number of doors is 6.</li>\n</ol>\n"
  }

  "example 269" in {
    test("- foo\n\n- bar\n\n\n- baz\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n</li>\n<li>\n<p>bar</p>\n</li>\n<li>\n<p>baz</p>\n</li>\n</ul>\n"
  }

  "example 270" in {
    test("- foo\n  - bar\n    - baz\n\n\n      bim\n") shouldBe "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>\n<p>baz</p>\n<p>bim</p>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
  }

  "example 271" in {
    test("- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim\n") shouldBe "<ul>\n<li>foo</li>\n<li>bar</li>\n</ul>\n<!-- -->\n<ul>\n<li>baz</li>\n<li>bim</li>\n</ul>\n"
  }

  "example 272" in {
    test("-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>notcode</p>\n</li>\n<li>\n<p>foo</p>\n</li>\n</ul>\n<!-- -->\n<pre><code>code\n</code></pre>\n"
  }

  //  "example 273" in {
  //    test( "- a\n - b\n  - c\n   - d\n    - e\n   - f\n  - g\n - h\n- i\n" ) shouldBe "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d</li>\n<li>e</li>\n<li>f</li>\n<li>g</li>\n<li>h</li>\n<li>i</li>\n</ul>\n"
  //  }

  //  "example 274" in {
  //    test( "1. a\n\n  2. b\n\n    3. c\n" ) shouldBe "<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ol>\n"
  //  }

  "example 275" in {
    test("- a\n- b\n\n- c\n") shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
  }

  //  "example 276" in {//todo lists
  //    test( "* a\n*\n\n* c\n" ) shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li></li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
  //  }

  "example 277" in {
    test("- a\n- b\n\n  c\n- d\n") shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
  }

  "example 278" in {
    test("- a\n- b\n\n  [ref]: /url\n- d\n") shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
  }

  "example 279" in {
    test("- a\n- ```\n  b\n\n\n  ```\n- c\n") shouldBe "<ul>\n<li>a</li>\n<li>\n<pre><code>b\n\n\n</code></pre>\n</li>\n<li>c</li>\n</ul>\n"
  }

  "example 280" in {
    test("- a\n  - b\n\n    c\n- d\n") shouldBe "<ul>\n<li>a\n<ul>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n</ul>\n</li>\n<li>d</li>\n</ul>\n"
  }

  "example 281" in {
    test("* a\n  > b\n  >\n* c\n") shouldBe "<ul>\n<li>a\n<blockquote>\n<p>b</p>\n</blockquote>\n</li>\n<li>c</li>\n</ul>\n"
  }

  "example 282" in {
    test("- a\n  > b\n  ```\n  c\n  ```\n- d\n") shouldBe "<ul>\n<li>a\n<blockquote>\n<p>b</p>\n</blockquote>\n<pre><code>c\n</code></pre>\n</li>\n<li>d</li>\n</ul>\n"
  }

  "example 283" in {
    test("- a\n") shouldBe "<ul>\n<li>a</li>\n</ul>\n"
  }

  "example 284" in {
    test("- a\n  - b\n") shouldBe "<ul>\n<li>a\n<ul>\n<li>b</li>\n</ul>\n</li>\n</ul>\n"
  }

  "example 285" in {
    test("1. ```\n   foo\n   ```\n\n   bar\n") shouldBe "<ol>\n<li>\n<pre><code>foo\n</code></pre>\n<p>bar</p>\n</li>\n</ol>\n"
  }

  //  "example 286" in {//todo: lists
  //    test( "* foo\n  * bar\n\n  baz\n" ) shouldBe "<ul>\n<li>\n<p>foo</p>\n<ul>\n<li>bar</li>\n</ul>\n<p>baz</p>\n</li>\n</ul>\n"
  //  }

  //  "example 287" in {//todo: lists
  //    test( "- a\n  - b\n  - c\n\n- d\n  - e\n  - f\n" ) shouldBe "<ul>\n<li>\n<p>a</p>\n<ul>\n<li>b</li>\n<li>c</li>\n</ul>\n</li>\n<li>\n<p>d</p>\n<ul>\n<li>e</li>\n<li>f</li>\n</ul>\n</li>\n</ul>\n"
  //  }

}
