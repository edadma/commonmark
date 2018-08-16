package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class Backslash_escapesSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 289" in {
    test( "\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~\n" ) shouldBe "<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>\n"
  }

  "example 290" in {
    test( "\\\t\\A\\a\\ \\3\\φ\\«\n" ) shouldBe "<p>\\\t\\A\\a\\ \\3\\φ\\«</p>\n"
  }

  "example 291" in {
    test( "\\*not emphasized*\n\\<br/> not a tag\n\\[not a link](/foo)\n\\`not code`\n1\\. not a list\n\\* not a list\n\\# not a heading\n\\[foo]: /url \"not a reference\"\n" ) shouldBe "<p>*not emphasized*\n&lt;br/&gt; not a tag\n[not a link](/foo)\n`not code`\n1. not a list\n* not a list\n# not a heading\n[foo]: /url &quot;not a reference&quot;</p>\n"
  }

//  "example 292" in {
//    test( "\\\\*emphasis*\n" ) shouldBe "<p>\\<em>emphasis</em></p>\n"
//  }

  "example 293" in {
    test( "foo\\\nbar\n" ) shouldBe "<p>foo<br />\nbar</p>\n"
  }

//  "example 294" in {//todo: inline
//    test( "`` \\[\\` ``\n" ) shouldBe "<p><code>\\[\\`</code></p>\n"
//  }

  "example 295" in {
    test( "    \\[\\]\n" ) shouldBe "<pre><code>\\[\\]\n</code></pre>\n"
  }

  "example 296" in {
    test( "~~~\n\\[\\]\n~~~\n" ) shouldBe "<pre><code>\\[\\]\n</code></pre>\n"
  }

//  "example 297" in {
//    test( "<http://example.com?find=\\*>\n" ) shouldBe "<p><a href=\"http://example.com?find=%5C*\">http://example.com?find=\\*</a></p>\n"
//  }

  "example 298" in {
    test( "<a href=\"/bar\\/)\">\n" ) shouldBe "<a href=\"/bar\\/)\">\n"
  }

//  "example 299" in {
//    test( "[foo](/bar\\* \"ti\\*tle\")\n" ) shouldBe "<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>\n"
//  }

//  "example 300" in {
//    test( "[foo]\n\n[foo]: /bar\\* \"ti\\*tle\"\n" ) shouldBe "<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>\n"
//  }

  "example 301" in {
    test( "``` foo\\+bar\nfoo\n```\n" ) shouldBe "<pre><code class=\"language-foo+bar\">foo\n</code></pre>\n"
  }

}
