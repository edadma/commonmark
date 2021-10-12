package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Fenced_code_blocksSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 119" in {
    test( "```\n<\n >\n```\n" ) shouldBe "<pre><code>&lt;\n &gt;\n</code></pre>\n"
  }

  "example 120" in {
    test( "~~~\n<\n >\n~~~\n" ) shouldBe "<pre><code>&lt;\n &gt;\n</code></pre>\n"
  }

  "example 121" in {
    test( "``\nfoo\n``\n" ) shouldBe "<p><code>foo</code></p>\n"
  }

  "example 122" in {
    test( "```\naaa\n~~~\n```\n" ) shouldBe "<pre><code>aaa\n~~~\n</code></pre>\n"
  }

  "example 123" in {
    test( "~~~\naaa\n```\n~~~\n" ) shouldBe "<pre><code>aaa\n```\n</code></pre>\n"
  }

  "example 124" in {
    test( "````\naaa\n```\n``````\n" ) shouldBe "<pre><code>aaa\n```\n</code></pre>\n"
  }

  "example 125" in {
    test( "~~~~\naaa\n~~~\n~~~~\n" ) shouldBe "<pre><code>aaa\n~~~\n</code></pre>\n"
  }

  "example 126" in {
    test( "```\n" ) shouldBe "<pre><code></code></pre>\n"
  }

  "example 127" in {
    test( "`````\n\n```\naaa\n" ) shouldBe "<pre><code>\n```\naaa\n</code></pre>\n"
  }

  "example 128" in {
    test( "> ```\n> aaa\n\nbbb\n" ) shouldBe "<blockquote>\n<pre><code>aaa\n</code></pre>\n</blockquote>\n<p>bbb</p>\n"
  }

  "example 129" in {
    test( "```\n\n  \n```\n" ) shouldBe "<pre><code>\n  \n</code></pre>\n"
  }

  "example 130" in {
    test( "```\n```\n" ) shouldBe "<pre><code></code></pre>\n"
  }

  "example 131" in {
    test( " ```\n aaa\naaa\n```\n" ) shouldBe "<pre><code>aaa\naaa\n</code></pre>\n"
  }

  "example 132" in {
    test( "  ```\naaa\n  aaa\naaa\n  ```\n" ) shouldBe "<pre><code>aaa\naaa\naaa\n</code></pre>\n"
  }

  "example 133" in {
    test( "   ```\n   aaa\n    aaa\n  aaa\n   ```\n" ) shouldBe "<pre><code>aaa\n aaa\naaa\n</code></pre>\n"
  }

  "example 134" in {
    test( "    ```\n    aaa\n    ```\n" ) shouldBe "<pre><code>```\naaa\n```\n</code></pre>\n"
  }

  "example 135" in {
    test( "```\naaa\n  ```\n" ) shouldBe "<pre><code>aaa\n</code></pre>\n"
  }

  "example 136" in {
    test( "   ```\naaa\n  ```\n" ) shouldBe "<pre><code>aaa\n</code></pre>\n"
  }

  "example 137" in {
    test( "```\naaa\n    ```\n" ) shouldBe "<pre><code>aaa\n    ```\n</code></pre>\n"
  }

  "example 138" in {
    test( "``` ```\naaa\n" ) shouldBe "<p><code> </code>\naaa</p>\n"
  }

  "example 139" in {
    test( "~~~~~~\naaa\n~~~ ~~\n" ) shouldBe "<pre><code>aaa\n~~~ ~~\n</code></pre>\n"
  }

  "example 140" in {
    test( "foo\n```\nbar\n```\nbaz\n" ) shouldBe "<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n"
  }

  "example 141" in {
    test( "foo\n---\n~~~\nbar\n~~~\n# baz\n" ) shouldBe "<h2>foo</h2>\n<pre><code>bar\n</code></pre>\n<h1>baz</h1>\n"
  }

  "example 142" in {
    test( "```ruby\ndef foo(x)\n  return 3\nend\n```\n" ) shouldBe "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
  }

  "example 143" in {
    test( "~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~\n" ) shouldBe "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
  }

  "example 144" in {
    test( "````;\n````\n" ) shouldBe "<pre><code class=\"language-;\"></code></pre>\n"
  }

  "example 145" in {
    test( "``` aa ```\nfoo\n" ) shouldBe "<p><code>aa</code>\nfoo</p>\n"
  }

  "example 146" in {
    test( "~~~ aa ``` ~~~\nfoo\n~~~\n" ) shouldBe "<pre><code class=\"language-aa\">foo\n</code></pre>\n"
  }

  "example 147" in {
    test( "```\n``` aaa\n```\n" ) shouldBe "<pre><code>``` aaa\n</code></pre>\n"
  }

}
