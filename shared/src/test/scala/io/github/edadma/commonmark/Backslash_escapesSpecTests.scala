package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Backslash_escapesSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 12" in {
    test( "\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~\n" ) shouldBe "<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>\n"
  }

  "example 13" in {
    test( "\\\t\\A\\a\\ \\3\\φ\\«\n" ) shouldBe "<p>\\\t\\A\\a\\ \\3\\φ\\«</p>\n"
  }

  "example 14" in {
    test( "\\*not emphasized*\n\\<br/> not a tag\n\\[not a link](/foo)\n\\`not code`\n1\\. not a list\n\\* not a list\n\\# not a heading\n\\[foo]: /url \"not a reference\"\n\\&ouml; not a character entity\n" ) shouldBe "<p>*not emphasized*\n&lt;br/&gt; not a tag\n[not a link](/foo)\n`not code`\n1. not a list\n* not a list\n# not a heading\n[foo]: /url &quot;not a reference&quot;\n&amp;ouml; not a character entity</p>\n"
  }

  "example 15" in {
    test( "\\\\*emphasis*\n" ) shouldBe "<p>\\<em>emphasis</em></p>\n"
  }

  "example 16" in {
    test( "foo\\\nbar\n" ) shouldBe "<p>foo<br />\nbar</p>\n"
  }

  "example 17" in {
    test( "`` \\[\\` ``\n" ) shouldBe "<p><code>\\[\\`</code></p>\n"
  }

  "example 18" in {
    test( "    \\[\\]\n" ) shouldBe "<pre><code>\\[\\]\n</code></pre>\n"
  }

  "example 19" in {
    test( "~~~\n\\[\\]\n~~~\n" ) shouldBe "<pre><code>\\[\\]\n</code></pre>\n"
  }

  "example 20" in {
    test( "<http://example.com?find=\\*>\n" ) shouldBe "<p><a href=\"http://example.com?find=%5C*\">http://example.com?find=\\*</a></p>\n"
  }

  "example 21" in {
    test( "<a href=\"/bar\\/)\">\n" ) shouldBe "<a href=\"/bar\\/)\">\n"
  }

  "example 22" in {
    test( "[foo](/bar\\* \"ti\\*tle\")\n" ) shouldBe "<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>\n"
  }

  "example 23" in {
    test( "[foo]\n\n[foo]: /bar\\* \"ti\\*tle\"\n" ) shouldBe "<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>\n"
  }

  "example 24" in {
    test( "``` foo\\+bar\nfoo\n```\n" ) shouldBe "<pre><code class=\"language-foo+bar\">foo\n</code></pre>\n"
  }

}
