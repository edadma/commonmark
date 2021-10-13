package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TabsSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 1" in {
    test("\tfoo\tbaz\t\tbim\n") shouldBe "<pre><code>foo\tbaz\t\tbim\n</code></pre>\n"
  }

  "example 2" in {
    test("  \tfoo\tbaz\t\tbim\n") shouldBe "<pre><code>foo\tbaz\t\tbim\n</code></pre>\n"
  }

  "example 3" in {
    test("    a\ta\n    ὐ\ta\n") shouldBe "<pre><code>a\ta\nὐ\ta\n</code></pre>\n"
  }

  "example 4" in {
    test("  - foo\n\n\tbar\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
  }

  "example 5" in {
    test("- foo\n\n\t\tbar\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<pre><code>  bar\n</code></pre>\n</li>\n</ul>\n"
  }

  "example 6" in {
    test(">\t\tfoo\n") shouldBe "<blockquote>\n<pre><code>  foo\n</code></pre>\n</blockquote>\n"
  }

  "example 7" in {
    test("-\t\tfoo\n") shouldBe "<ul>\n<li>\n<pre><code>  foo\n</code></pre>\n</li>\n</ul>\n"
  }

  "example 8" in {
    test("    foo\n\tbar\n") shouldBe "<pre><code>foo\nbar\n</code></pre>\n"
  }

  "example 9" in {
    test(" - foo\n   - bar\n\t - baz\n") shouldBe "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>baz</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
  }

  "example 10" in {
    test("#\tFoo\n") shouldBe "<h1>Foo</h1>\n"
  }

  "example 11" in {
    test("*\t*\t*\t\n") shouldBe "<hr />\n"
  }

}
