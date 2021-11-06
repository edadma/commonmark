package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Link_reference_definitionsSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 192" in {
    test( "[foo]: /url \"title\"\n\n[foo]\n" ) shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }

  "example 193" in {
    test( "   [foo]: \n      /url  \n           'the title'  \n\n[foo]\n" ) shouldBe "<p><a href=\"/url\" title=\"the title\">foo</a></p>\n"
  }

  "example 194" in {
    test( "[Foo*bar\\]]:my_(url) 'title (with parens)'\n\n[Foo*bar\\]]\n" ) shouldBe "<p><a href=\"my_(url)\" title=\"title (with parens)\">Foo*bar]</a></p>\n"
  }

  "example 195" in {
    test( "[Foo bar]:\n<my url>\n'title'\n\n[Foo bar]\n" ) shouldBe "<p><a href=\"my%20url\" title=\"title\">Foo bar</a></p>\n"
  }

  "example 196" in {
    test( "[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]\n" ) shouldBe "<p><a href=\"/url\" title=\"\ntitle\nline1\nline2\n\">foo</a></p>\n"
  }

  "example 197" in {
    test( "[foo]: /url 'title\n\nwith blank line'\n\n[foo]\n" ) shouldBe "<p>[foo]: /url 'title</p>\n<p>with blank line'</p>\n<p>[foo]</p>\n"
  }

  "example 198" in {
    test( "[foo]:\n/url\n\n[foo]\n" ) shouldBe "<p><a href=\"/url\">foo</a></p>\n"
  }

  "example 199" in {
    test( "[foo]:\n\n[foo]\n" ) shouldBe "<p>[foo]:</p>\n<p>[foo]</p>\n"
  }

  "example 200" in {
    test( "[foo]: <>\n\n[foo]\n" ) shouldBe "<p><a href=\"\">foo</a></p>\n"
  }

  "example 201" in {
    test( "[foo]: <bar>(baz)\n\n[foo]\n" ) shouldBe "<p>[foo]: <bar>(baz)</p>\n<p>[foo]</p>\n"
  }

  "example 202" in {
    test( "[foo]: /url\\bar\\*baz \"foo\\\"bar\\baz\"\n\n[foo]\n" ) shouldBe "<p><a href=\"/url%5Cbar*baz\" title=\"foo&quot;bar\\baz\">foo</a></p>\n"
  }

  "example 203" in {
    test( "[foo]\n\n[foo]: url\n" ) shouldBe "<p><a href=\"url\">foo</a></p>\n"
  }

  "example 204" in {
    test( "[foo]\n\n[foo]: first\n[foo]: second\n" ) shouldBe "<p><a href=\"first\">foo</a></p>\n"
  }

  "example 205" in {
    test( "[FOO]: /url\n\n[Foo]\n" ) shouldBe "<p><a href=\"/url\">Foo</a></p>\n"
  }

  "example 206" in {
    test( "[ΑΓΩ]: /φου\n\n[αγω]\n" ) shouldBe "<p><a href=\"/%CF%86%CE%BF%CF%85\">αγω</a></p>\n"
  }

  "example 207" in {
    test( "[foo]: /url\n" ) shouldBe ""
  }

  "example 208" in {
    test( "[\nfoo\n]: /url\nbar\n" ) shouldBe "<p>bar</p>\n"
  }

  "example 209" in {
    test( "[foo]: /url \"title\" ok\n" ) shouldBe "<p>[foo]: /url &quot;title&quot; ok</p>\n"
  }

  "example 210" in {
    test( "[foo]: /url\n\"title\" ok\n" ) shouldBe "<p>&quot;title&quot; ok</p>\n"
  }

  "example 211" in {
    test( "    [foo]: /url \"title\"\n\n[foo]\n" ) shouldBe "<pre><code>[foo]: /url &quot;title&quot;\n</code></pre>\n<p>[foo]</p>\n"
  }

  "example 212" in {
    test( "```\n[foo]: /url\n```\n\n[foo]\n" ) shouldBe "<pre><code>[foo]: /url\n</code></pre>\n<p>[foo]</p>\n"
  }

  "example 213" in {
    test( "Foo\n[bar]: /baz\n\n[bar]\n" ) shouldBe "<p>Foo\n[bar]: /baz</p>\n<p>[bar]</p>\n"
  }

  "example 214" in {
    test( "# [Foo]\n[foo]: /url\n> bar\n" ) shouldBe "<h1><a href=\"/url\">Foo</a></h1>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
  }

  "example 215" in {
    test( "[foo]: /url\nbar\n===\n[foo]\n" ) shouldBe "<h1>bar</h1>\n<p><a href=\"/url\">foo</a></p>\n"
  }

  "example 216" in {
    test( "[foo]: /url\n===\n[foo]\n" ) shouldBe "<p>===\n<a href=\"/url\">foo</a></p>\n"
  }

  "example 217" in {
    test( "[foo]: /foo-url \"foo\"\n[bar]: /bar-url\n  \"bar\"\n[baz]: /baz-url\n\n[foo],\n[bar],\n[baz]\n" ) shouldBe "<p><a href=\"/foo-url\" title=\"foo\">foo</a>,\n<a href=\"/bar-url\" title=\"bar\">bar</a>,\n<a href=\"/baz-url\">baz</a></p>\n"
  }

  "example 218" in {
    test( "[foo]\n\n> [foo]: /url\n" ) shouldBe "<p><a href=\"/url\">foo</a></p>\n<blockquote>\n</blockquote>\n"
  }

}
