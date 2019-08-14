package xyz.hyperreal.commonmark

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class Link_reference_definitionsSpecTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {
/*
  "example 159" in {
    test( "[foo]: /url \"title\"\n\n[foo]\n" ) shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }

  "example 160" in {
    test( "   [foo]: \n      /url  \n           'the title'  \n\n[foo]\n" ) shouldBe "<p><a href=\"/url\" title=\"the title\">foo</a></p>\n"
  }

  "example 161" in {
    test( "[Foo*bar\\]]:my_(url) 'title (with parens)'\n\n[Foo*bar\\]]\n" ) shouldBe "<p><a href=\"my_(url)\" title=\"title (with parens)\">Foo*bar]</a></p>\n"
  }

  "example 162" in {
    test( "[Foo bar]:\n<my%20url>\n'title'\n\n[Foo bar]\n" ) shouldBe "<p><a href=\"my%20url\" title=\"title\">Foo bar</a></p>\n"
  }

  "example 163" in {
    test( "[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]\n" ) shouldBe "<p><a href=\"/url\" title=\"\ntitle\nline1\nline2\n\">foo</a></p>\n"
  }

  "example 164" in {
    test( "[foo]: /url 'title\n\nwith blank line'\n\n[foo]\n" ) shouldBe "<p>[foo]: /url 'title</p>\n<p>with blank line'</p>\n<p>[foo]</p>\n"
  }

  "example 165" in {
    test( "[foo]:\n/url\n\n[foo]\n" ) shouldBe "<p><a href=\"/url\">foo</a></p>\n"
  }

  "example 166" in {
    test( "[foo]:\n\n[foo]\n" ) shouldBe "<p>[foo]:</p>\n<p>[foo]</p>\n"
  }

  "example 167" in {
    test( "[foo]: /url\\bar\\*baz \"foo\\\"bar\\baz\"\n\n[foo]\n" ) shouldBe "<p><a href=\"/url%5Cbar*baz\" title=\"foo&quot;bar\\baz\">foo</a></p>\n"
  }

  "example 168" in {
    test( "[foo]\n\n[foo]: url\n" ) shouldBe "<p><a href=\"url\">foo</a></p>\n"
  }

  "example 169" in {
    test( "[foo]\n\n[foo]: first\n[foo]: second\n" ) shouldBe "<p><a href=\"first\">foo</a></p>\n"
  }

  "example 170" in {
    test( "[FOO]: /url\n\n[Foo]\n" ) shouldBe "<p><a href=\"/url\">Foo</a></p>\n"
  }

  "example 171" in {
    test( "[ΑΓΩ]: /φου\n\n[αγω]\n" ) shouldBe "<p><a href=\"/%CF%86%CE%BF%CF%85\">αγω</a></p>\n"
  }

  "example 172" in {
    test( "[foo]: /url\n" ) shouldBe ""
  }

  "example 173" in {
    test( "[\nfoo\n]: /url\nbar\n" ) shouldBe "<p>bar</p>\n"
  }

  "example 174" in {
    test( "[foo]: /url \"title\" ok\n" ) shouldBe "<p>[foo]: /url &quot;title&quot; ok</p>\n"
  }

  "example 175" in {
    test( "[foo]: /url\n\"title\" ok\n" ) shouldBe "<p>&quot;title&quot; ok</p>\n"
  }

  "example 176" in {
    test( "    [foo]: /url \"title\"\n\n[foo]\n" ) shouldBe "<pre><code>[foo]: /url &quot;title&quot;\n</code></pre>\n<p>[foo]</p>\n"
  }

  "example 177" in {
    test( "```\n[foo]: /url\n```\n\n[foo]\n" ) shouldBe "<pre><code>[foo]: /url\n</code></pre>\n<p>[foo]</p>\n"
  }

  "example 178" in {
    test( "Foo\n[bar]: /baz\n\n[bar]\n" ) shouldBe "<p>Foo\n[bar]: /baz</p>\n<p>[bar]</p>\n"
  }

  "example 179" in {
    test( "# [Foo]\n[foo]: /url\n> bar\n" ) shouldBe "<h1><a href=\"/url\">Foo</a></h1>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
  }

  "example 180" in {
    test( "[foo]: /foo-url \"foo\"\n[bar]: /bar-url\n  \"bar\"\n[baz]: /baz-url\n\n[foo],\n[bar],\n[baz]\n" ) shouldBe "<p><a href=\"/foo-url\" title=\"foo\">foo</a>,\n<a href=\"/bar-url\" title=\"bar\">bar</a>,\n<a href=\"/baz-url\">baz</a></p>\n"
  }

  "example 181" in {
    test( "[foo]\n\n> [foo]: /url\n" ) shouldBe "<p><a href=\"/url\">foo</a></p>\n<blockquote>\n</blockquote>\n"
  }
*/
}
