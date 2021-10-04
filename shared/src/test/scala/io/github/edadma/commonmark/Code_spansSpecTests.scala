package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Code_spansSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 328" in {
    test( "`foo`\n" ) shouldBe "<p><code>foo</code></p>\n"
  }

  "example 329" in {
    test( "`` foo ` bar ``\n" ) shouldBe "<p><code>foo ` bar</code></p>\n"
  }

  "example 330" in {
    test( "` `` `\n" ) shouldBe "<p><code>``</code></p>\n"
  }

  "example 331" in {
    test( "`  ``  `\n" ) shouldBe "<p><code> `` </code></p>\n"
  }

  "example 332" in {
    test( "` a`\n" ) shouldBe "<p><code> a</code></p>\n"
  }

  "example 333" in {
    test( "` b `\n" ) shouldBe "<p><code> b </code></p>\n"
  }

  "example 334" in {
    test( "` `\n`  `\n" ) shouldBe "<p><code> </code>\n<code>  </code></p>\n"
  }

  "example 335" in {
    test( "``\nfoo\nbar  \nbaz\n``\n" ) shouldBe "<p><code>foo bar   baz</code></p>\n"
  }

  "example 336" in {
    test( "``\nfoo \n``\n" ) shouldBe "<p><code>foo </code></p>\n"
  }

  "example 337" in {
    test( "`foo   bar \nbaz`\n" ) shouldBe "<p><code>foo   bar  baz</code></p>\n"
  }

  "example 338" in {
    test( "`foo\\`bar`\n" ) shouldBe "<p><code>foo\\</code>bar`</p>\n"
  }

  "example 339" in {
    test( "``foo`bar``\n" ) shouldBe "<p><code>foo`bar</code></p>\n"
  }

  "example 340" in {
    test( "` foo `` bar `\n" ) shouldBe "<p><code>foo `` bar</code></p>\n"
  }

  "example 341" in {
    test( "*foo`*`\n" ) shouldBe "<p>*foo<code>*</code></p>\n"
  }

  "example 342" in {
    test( "[not a `link](/foo`)\n" ) shouldBe "<p>[not a <code>link](/foo</code>)</p>\n"
  }

  "example 343" in {
    test( "`<a href=\"`\">`\n" ) shouldBe "<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>\n"
  }

  "example 344" in {
    test( "<a href=\"`\">`\n" ) shouldBe "<p><a href=\"`\">`</p>\n"
  }

  "example 345" in {
    test( "`<http://foo.bar.`baz>`\n" ) shouldBe "<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>\n"
  }

  "example 346" in {
    test( "<http://foo.bar.`baz>`\n" ) shouldBe "<p><a href=\"http://foo.bar.%60baz\">http://foo.bar.`baz</a>`</p>\n"
  }

  "example 347" in {
    test( "```foo``\n" ) shouldBe "<p>```foo``</p>\n"
  }

  "example 348" in {
    test( "`foo\n" ) shouldBe "<p>`foo</p>\n"
  }

  "example 349" in {
    test( "`foo``bar``\n" ) shouldBe "<p>`foo<code>bar</code></p>\n"
  }

}
