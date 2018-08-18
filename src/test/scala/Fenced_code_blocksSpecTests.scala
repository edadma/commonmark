package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class Fenced_code_blocksSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 88" in {
    test( "```\n<\n >\n```\n" ) shouldBe "<pre><code>&lt;\n &gt;\n</code></pre>\n"
  }

  "example 89" in {
    test( "~~~\n<\n >\n~~~\n" ) shouldBe "<pre><code>&lt;\n &gt;\n</code></pre>\n"
  }

  "example 90" in {
    test( "``\nfoo\n``\n" ) shouldBe "<p><code>foo</code></p>\n"
  }

  "example 91" in {
    test( "```\naaa\n~~~\n```\n" ) shouldBe "<pre><code>aaa\n~~~\n</code></pre>\n"
  }

  "example 92" in {
    test( "~~~\naaa\n```\n~~~\n" ) shouldBe "<pre><code>aaa\n```\n</code></pre>\n"
  }

  "example 93" in {
    test( "````\naaa\n```\n``````\n" ) shouldBe "<pre><code>aaa\n```\n</code></pre>\n"
  }

  "example 94" in {
    test( "~~~~\naaa\n~~~\n~~~~\n" ) shouldBe "<pre><code>aaa\n~~~\n</code></pre>\n"
  }

  "example 95" in {
    test( "```\n" ) shouldBe "<pre><code></code></pre>\n"
  }

  "example 96" in {
    test( "`````\n\n```\naaa\n" ) shouldBe "<pre><code>\n```\naaa\n</code></pre>\n"
  }

  "example 97" in {
    test( "> ```\n> aaa\n\nbbb\n" ) shouldBe "<blockquote>\n<pre><code>aaa\n</code></pre>\n</blockquote>\n<p>bbb</p>\n"
  }

  "example 98" in {
    test( "```\n\n  \n```\n" ) shouldBe "<pre><code>\n  \n</code></pre>\n"
  }

  "example 99" in {
    test( "```\n```\n" ) shouldBe "<pre><code></code></pre>\n"
  }

  "example 100" in {
    test( " ```\n aaa\naaa\n```\n" ) shouldBe "<pre><code>aaa\naaa\n</code></pre>\n"
  }

  "example 101" in {
    test( "  ```\naaa\n  aaa\naaa\n  ```\n" ) shouldBe "<pre><code>aaa\naaa\naaa\n</code></pre>\n"
  }

  "example 102" in {
    test( "   ```\n   aaa\n    aaa\n  aaa\n   ```\n" ) shouldBe "<pre><code>aaa\n aaa\naaa\n</code></pre>\n"
  }

  "example 103" in {
    test( "    ```\n    aaa\n    ```\n" ) shouldBe "<pre><code>```\naaa\n```\n</code></pre>\n"
  }

  "example 104" in {
    test( "```\naaa\n  ```\n" ) shouldBe "<pre><code>aaa\n</code></pre>\n"
  }

  "example 105" in {
    test( "   ```\naaa\n  ```\n" ) shouldBe "<pre><code>aaa\n</code></pre>\n"
  }

  "example 106" in {
    test( "```\naaa\n    ```\n" ) shouldBe "<pre><code>aaa\n    ```\n</code></pre>\n"
  }

  "example 107" in {
    test( "``` ```\naaa\n" ) shouldBe "<p><code></code>\naaa</p>\n"
  }

  "example 108" in {
    test( "~~~~~~\naaa\n~~~ ~~\n" ) shouldBe "<pre><code>aaa\n~~~ ~~\n</code></pre>\n"
  }

  "example 109" in {
    test( "foo\n```\nbar\n```\nbaz\n" ) shouldBe "<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n"
  }

  "example 110" in {
    test( "foo\n---\n~~~\nbar\n~~~\n# baz\n" ) shouldBe "<h2>foo</h2>\n<pre><code>bar\n</code></pre>\n<h1>baz</h1>\n"
  }

  "example 111" in {
    test( "```ruby\ndef foo(x)\n  return 3\nend\n```\n" ) shouldBe "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
  }

  "example 112" in {
    test( "~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~\n" ) shouldBe "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
  }

  "example 113" in {
    test( "````;\n````\n" ) shouldBe "<pre><code class=\"language-;\"></code></pre>\n"
  }

  "example 114" in {
    test( "``` aa ```\nfoo\n" ) shouldBe "<p><code>aa</code>\nfoo</p>\n"
  }

  "example 115" in {
    test( "```\n``` aaa\n```\n" ) shouldBe "<pre><code>``` aaa\n</code></pre>\n"
  }

}
