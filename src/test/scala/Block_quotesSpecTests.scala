package xyz.hyperreal.commonmark

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class Block_quotesSpecTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

  "example 191" in {
    test( "> # Foo\n> bar\n> baz\n" ) shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }

  "example 192" in {
    test( "># Foo\n>bar\n> baz\n" ) shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }

  "example 193" in {
    test( "   > # Foo\n   > bar\n > baz\n" ) shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }

  "example 194" in {
    test( "    > # Foo\n    > bar\n    > baz\n" ) shouldBe "<pre><code>&gt; # Foo\n&gt; bar\n&gt; baz\n</code></pre>\n"
  }

  "example 195" in {
    test( "> # Foo\n> bar\nbaz\n" ) shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }

  "example 196" in {
    test( "> bar\nbaz\n> foo\n" ) shouldBe "<blockquote>\n<p>bar\nbaz\nfoo</p>\n</blockquote>\n"
  }

  "example 197" in {
    test( "> foo\n---\n" ) shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr />\n"
  }

  "example 198" in {
    test( "> - foo\n- bar\n" ) shouldBe "<blockquote>\n<ul>\n<li>foo</li>\n</ul>\n</blockquote>\n<ul>\n<li>bar</li>\n</ul>\n"
  }

  "example 199" in {
    test( ">     foo\n    bar\n" ) shouldBe "<blockquote>\n<pre><code>foo\n</code></pre>\n</blockquote>\n<pre><code>bar\n</code></pre>\n"
  }

  "example 200" in {
    test( "> ```\nfoo\n```\n" ) shouldBe "<blockquote>\n<pre><code></code></pre>\n</blockquote>\n<p>foo</p>\n<pre><code></code></pre>\n"
  }

//  "example 201" in {//todo: continuation paragraph that looks like an indented block
//    test( "> foo\n    - bar\n" ) shouldBe "<blockquote>\n<p>foo\n- bar</p>\n</blockquote>\n"
//  }

  "example 202" in {
    test( ">\n" ) shouldBe "<blockquote>\n</blockquote>\n"
  }

  "example 203" in {
    test( ">\n>  \n> \n" ) shouldBe "<blockquote>\n</blockquote>\n"
  }

  "example 204" in {
    test( ">\n> foo\n>  \n" ) shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n"
  }

  "example 205" in {
    test( "> foo\n\n> bar\n" ) shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
  }

  "example 206" in {
    test( "> foo\n> bar\n" ) shouldBe "<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n"
  }

  "example 207" in {
    test( "> foo\n>\n> bar\n" ) shouldBe "<blockquote>\n<p>foo</p>\n<p>bar</p>\n</blockquote>\n"
  }

  "example 208" in {
    test( "foo\n> bar\n" ) shouldBe "<p>foo</p>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
  }

  "example 209" in {
    test( "> aaa\n***\n> bbb\n" ) shouldBe "<blockquote>\n<p>aaa</p>\n</blockquote>\n<hr />\n<blockquote>\n<p>bbb</p>\n</blockquote>\n"
  }

  "example 210" in {
    test( "> bar\nbaz\n" ) shouldBe "<blockquote>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }

  "example 211" in {
    test( "> bar\n\nbaz\n" ) shouldBe "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
  }

  "example 212" in {
    test( "> bar\n>\nbaz\n" ) shouldBe "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
  }

  "example 213" in {
    test( "> > > foo\nbar\n" ) shouldBe "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"
  }

  "example 214" in {
    test( ">>> foo\n> bar\n>>baz\n" ) shouldBe "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar\nbaz</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"
  }

  "example 215" in {
    test( ">     code\n\n>    not code\n" ) shouldBe "<blockquote>\n<pre><code>code\n</code></pre>\n</blockquote>\n<blockquote>\n<p>not code</p>\n</blockquote>\n"
  }

}
