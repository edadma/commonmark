package io.github.edadma.commonmark

import org.scalatest._
import prop.PropertyChecks

class Code_spansSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 314" in {
    test("`foo`\n") shouldBe "<p><code>foo</code></p>\n"
  }

  "example 315" in {
    test("`` foo ` bar  ``\n") shouldBe "<p><code>foo ` bar</code></p>\n"
  }

  "example 316" in {
    test("` `` `\n") shouldBe "<p><code>``</code></p>\n"
  }

  "example 317" in {
    test("``\nfoo\n``\n") shouldBe "<p><code>foo</code></p>\n"
  }

  "example 318" in {
    test("`foo   bar\n  baz`\n") shouldBe "<p><code>foo bar baz</code></p>\n"
  }

  "example 319" in {
    test("`a  b`\n") shouldBe "<p><code>a  b</code></p>\n"
  }

  "example 320" in {
    test("`foo `` bar`\n") shouldBe "<p><code>foo `` bar</code></p>\n"
  }

  "example 321" in {
    test("`foo\\`bar`\n") shouldBe "<p><code>foo\\</code>bar`</p>\n"
  }

  "example 322" in {
    test("*foo`*`\n") shouldBe "<p>*foo<code>*</code></p>\n"
  }

  "example 323" in {
    test("[not a `link](/foo`)\n") shouldBe "<p>[not a <code>link](/foo</code>)</p>\n"
  }

  "example 324" in {
    test("`<a href=\"`\">`\n") shouldBe "<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>\n"
  }

  "example 325" in {
    test("<a href=\"`\">`\n") shouldBe "<p><a href=\"`\">`</p>\n"
  }

  "example 326" in {
    test("`<http://foo.bar.`baz>`\n") shouldBe "<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>\n"
  }

  "example 327" in {
    test("<http://foo.bar.`baz>`\n") shouldBe "<p><a href=\"http://foo.bar.%60baz\">http://foo.bar.`baz</a>`</p>\n"
  }

  "example 328" in {
    test("```foo``\n") shouldBe "<p>```foo``</p>\n"
  }

  "example 329" in {
    test("`foo\n") shouldBe "<p>`foo</p>\n"
  }

  "example 330" in {
    test("`foo``bar``\n") shouldBe "<p>`foo<code>bar</code></p>\n"
  }

}
