package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Block_quotesSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 228" in {
    test( "> # Foo\n> bar\n> baz\n" ) shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }

  "example 229" in {
    test( "># Foo\n>bar\n> baz\n" ) shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }

  "example 230" in {
    test( "   > # Foo\n   > bar\n > baz\n" ) shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }

  "example 231" in {
    test( "    > # Foo\n    > bar\n    > baz\n" ) shouldBe "<pre><code>&gt; # Foo\n&gt; bar\n&gt; baz\n</code></pre>\n"
  }

  "example 232" in {
    test( "> # Foo\n> bar\nbaz\n" ) shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }

  "example 233" in {
    test( "> bar\nbaz\n> foo\n" ) shouldBe "<blockquote>\n<p>bar\nbaz\nfoo</p>\n</blockquote>\n"
  }

  "example 234" in {
    test( "> foo\n---\n" ) shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr />\n"
  }

  "example 235" in {
    test( "> - foo\n- bar\n" ) shouldBe "<blockquote>\n<ul>\n<li>foo</li>\n</ul>\n</blockquote>\n<ul>\n<li>bar</li>\n</ul>\n"
  }

  "example 236" in {
    test( ">     foo\n    bar\n" ) shouldBe "<blockquote>\n<pre><code>foo\n</code></pre>\n</blockquote>\n<pre><code>bar\n</code></pre>\n"
  }

  "example 237" in {
    test( "> ```\nfoo\n```\n" ) shouldBe "<blockquote>\n<pre><code></code></pre>\n</blockquote>\n<p>foo</p>\n<pre><code></code></pre>\n"
  }

  "example 238" in {
    test( "> foo\n    - bar\n" ) shouldBe "<blockquote>\n<p>foo\n- bar</p>\n</blockquote>\n"
  }

  "example 239" in {
    test( ">\n" ) shouldBe "<blockquote>\n</blockquote>\n"
  }

  "example 240" in {
    test( ">\n>  \n> \n" ) shouldBe "<blockquote>\n</blockquote>\n"
  }

  "example 241" in {
    test( ">\n> foo\n>  \n" ) shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n"
  }

  "example 242" in {
    test( "> foo\n\n> bar\n" ) shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
  }

  "example 243" in {
    test( "> foo\n> bar\n" ) shouldBe "<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n"
  }

  "example 244" in {
    test( "> foo\n>\n> bar\n" ) shouldBe "<blockquote>\n<p>foo</p>\n<p>bar</p>\n</blockquote>\n"
  }

  "example 245" in {
    test( "foo\n> bar\n" ) shouldBe "<p>foo</p>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
  }

  "example 246" in {
    test( "> aaa\n***\n> bbb\n" ) shouldBe "<blockquote>\n<p>aaa</p>\n</blockquote>\n<hr />\n<blockquote>\n<p>bbb</p>\n</blockquote>\n"
  }

  "example 247" in {
    test( "> bar\nbaz\n" ) shouldBe "<blockquote>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }

  "example 248" in {
    test( "> bar\n\nbaz\n" ) shouldBe "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
  }

  "example 249" in {
    test( "> bar\n>\nbaz\n" ) shouldBe "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
  }

  "example 250" in {
    test( "> > > foo\nbar\n" ) shouldBe "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"
  }

  "example 251" in {
    test( ">>> foo\n> bar\n>>baz\n" ) shouldBe "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar\nbaz</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"
  }

  "example 252" in {
    test( ">     code\n\n>    not code\n" ) shouldBe "<blockquote>\n<pre><code>code\n</code></pre>\n</blockquote>\n<blockquote>\n<p>not code</p>\n</blockquote>\n"
  }

}
