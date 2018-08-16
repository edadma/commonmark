package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class Entity_and_numeric_character_referencesSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 302" in {
    test( "&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;\n" ) shouldBe "<p>  &amp; © Æ Ď\n¾ ℋ ⅆ\n∲ ≧̸</p>\n"
  }

  "example 303" in {
    test( "&#35; &#1234; &#992; &#98765432; &#0;\n" ) shouldBe "<p># Ӓ Ϡ � �</p>\n"
  }
/*
  "example 304" in {
    test( "&#X22; &#XD06; &#xcab;\n" ) shouldBe "<p>&quot; ആ ಫ</p>\n"
  }

  "example 305" in {
    test( "&nbsp &x; &#; &#x;\n&ThisIsNotDefined; &hi?;\n" ) shouldBe "<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;\n&amp;ThisIsNotDefined; &amp;hi?;</p>\n"
  }

  "example 306" in {
    test( "&copy\n" ) shouldBe "<p>&amp;copy</p>\n"
  }

  "example 307" in {
    test( "&MadeUpEntity;\n" ) shouldBe "<p>&amp;MadeUpEntity;</p>\n"
  }

//  "example 308" in {
//    test( "<a href=\"&ouml;&ouml;.html\">\n" ) shouldBe "<a href=\"&ouml;&ouml;.html\">\n"
//  }

//  "example 309" in {
//    test( "[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")\n" ) shouldBe "<p><a href=\"/f%C3%B6%C3%B6\" title=\"föö\">foo</a></p>\n"
//  }

//  "example 310" in {
//    test( "[foo]\n\n[foo]: /f&ouml;&ouml; \"f&ouml;&ouml;\"\n" ) shouldBe "<p><a href=\"/f%C3%B6%C3%B6\" title=\"föö\">foo</a></p>\n"
//  }

  "example 311" in {
    test( "``` f&ouml;&ouml;\nfoo\n```\n" ) shouldBe "<pre><code class=\"language-föö\">foo\n</code></pre>\n"
  }

//  "example 312" in {
//    test( "`f&ouml;&ouml;`\n" ) shouldBe "<p><code>f&amp;ouml;&amp;ouml;</code></p>\n"
//  }

  "example 313" in {
    test( "    f&ouml;f&ouml;\n" ) shouldBe "<pre><code>f&amp;ouml;f&amp;ouml;\n</code></pre>\n"
  }
*/
}
