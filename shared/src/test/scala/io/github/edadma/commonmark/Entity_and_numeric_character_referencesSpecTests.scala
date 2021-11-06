package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Entity_and_numeric_character_referencesSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 25" in {
    test( "&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;\n" ) shouldBe "<p>  &amp; © Æ Ď\n¾ ℋ ⅆ\n∲ ≧̸</p>\n"
  }

  "example 26" in {
    test( "&#35; &#1234; &#992; &#0;\n" ) shouldBe "<p># Ӓ Ϡ �</p>\n"
  }

  "example 27" in {
    test( "&#X22; &#XD06; &#xcab;\n" ) shouldBe "<p>&quot; ആ ಫ</p>\n"
  }

  "example 28" in {
    test( "&nbsp &x; &#; &#x;\n&#87654321;\n&#abcdef0;\n&ThisIsNotDefined; &hi?;\n" ) shouldBe "<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;\n&amp;#87654321;\n&amp;#abcdef0;\n&amp;ThisIsNotDefined; &amp;hi?;</p>\n"
  }

  "example 29" in {
    test( "&copy\n" ) shouldBe "<p>&amp;copy</p>\n"
  }

  "example 30" in {
    test( "&MadeUpEntity;\n" ) shouldBe "<p>&amp;MadeUpEntity;</p>\n"
  }

  "example 31" in {
    test( "<a href=\"&ouml;&ouml;.html\">\n" ) shouldBe "<a href=\"&ouml;&ouml;.html\">\n"
  }

  "example 32" in {
    test( "[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")\n" ) shouldBe "<p><a href=\"/f%C3%B6%C3%B6\" title=\"föö\">foo</a></p>\n"
  }

  "example 33" in {
    test( "[foo]\n\n[foo]: /f&ouml;&ouml; \"f&ouml;&ouml;\"\n" ) shouldBe "<p><a href=\"/f%C3%B6%C3%B6\" title=\"föö\">foo</a></p>\n"
  }

  "example 34" in {
    test( "``` f&ouml;&ouml;\nfoo\n```\n" ) shouldBe "<pre><code class=\"language-föö\">foo\n</code></pre>\n"
  }

  "example 35" in {
    test( "`f&ouml;&ouml;`\n" ) shouldBe "<p><code>f&amp;ouml;&amp;ouml;</code></p>\n"
  }

  "example 36" in {
    test( "    f&ouml;f&ouml;\n" ) shouldBe "<pre><code>f&amp;ouml;f&amp;ouml;\n</code></pre>\n"
  }

  "example 37" in {
    test( "&#42;foo&#42;\n*foo*\n" ) shouldBe "<p>*foo*\n<em>foo</em></p>\n"
  }

  "example 38" in {
    test( "&#42; foo\n\n* foo\n" ) shouldBe "<p>* foo</p>\n<ul>\n<li>foo</li>\n</ul>\n"
  }

  "example 39" in {
    test( "foo&#10;&#10;bar\n" ) shouldBe "<p>foo\n\nbar</p>\n"
  }

  "example 40" in {
    test( "&#9;foo\n" ) shouldBe "<p>\tfoo</p>\n"
  }

  "example 41" in {
    test( "[a](url &quot;tit&quot;)\n" ) shouldBe "<p>[a](url &quot;tit&quot;)</p>\n"
  }

}
