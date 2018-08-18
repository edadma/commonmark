package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class Raw_HTMLSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 584" in {
    test( "<a><bab><c2c>\n" ) shouldBe "<p><a><bab><c2c></p>\n"
  }

  "example 585" in {
    test( "<a/><b2/>\n" ) shouldBe "<p><a/><b2/></p>\n"
  }

  "example 586" in {
    test( "<a  /><b2\ndata=\"foo\" >\n" ) shouldBe "<p><a  /><b2\ndata=\"foo\" ></p>\n"
  }

  "example 587" in {
    test( "<a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 />\n" ) shouldBe "<p><a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 /></p>\n"
  }

  "example 588" in {
    test( "Foo <responsive-image src=\"foo.jpg\" />\n" ) shouldBe "<p>Foo <responsive-image src=\"foo.jpg\" /></p>\n"
  }

  "example 589" in {
    test( "<33> <__>\n" ) shouldBe "<p>&lt;33&gt; &lt;__&gt;</p>\n"
  }

  "example 590" in {
    test( "<a h*#ref=\"hi\">\n" ) shouldBe "<p>&lt;a h*#ref=&quot;hi&quot;&gt;</p>\n"
  }

  "example 591" in {
    test( "<a href=\"hi'> <a href=hi'>\n" ) shouldBe "<p>&lt;a href=&quot;hi'&gt; &lt;a href=hi'&gt;</p>\n"
  }

  "example 592" in {
    test( "< a><\nfoo><bar/ >\n" ) shouldBe "<p>&lt; a&gt;&lt;\nfoo&gt;&lt;bar/ &gt;</p>\n"
  }

  "example 593" in {
    test( "<a href='bar'title=title>\n" ) shouldBe "<p>&lt;a href='bar'title=title&gt;</p>\n"
  }

  "example 594" in {
    test( "</a></foo >\n" ) shouldBe "<p></a></foo ></p>\n"
  }

  "example 595" in {
    test( "</a href=\"foo\">\n" ) shouldBe "<p>&lt;/a href=&quot;foo&quot;&gt;</p>\n"
  }

  "example 596" in {
    test( "foo <!-- this is a\ncomment - with hyphen -->\n" ) shouldBe "<p>foo <!-- this is a\ncomment - with hyphen --></p>\n"
  }

  "example 597" in {
    test( "foo <!-- not a comment -- two hyphens -->\n" ) shouldBe "<p>foo &lt;!-- not a comment -- two hyphens --&gt;</p>\n"
  }

  "example 598" in {
    test( "foo <!--> foo -->\n\nfoo <!-- foo--->\n" ) shouldBe "<p>foo &lt;!--&gt; foo --&gt;</p>\n<p>foo &lt;!-- foo---&gt;</p>\n"
  }

  "example 599" in {
    test( "foo <?php echo $a; ?>\n" ) shouldBe "<p>foo <?php echo $a; ?></p>\n"
  }

  "example 600" in {
    test( "foo <!ELEMENT br EMPTY>\n" ) shouldBe "<p>foo <!ELEMENT br EMPTY></p>\n"
  }

  "example 601" in {
    test( "foo <![CDATA[>&<]]>\n" ) shouldBe "<p>foo <![CDATA[>&<]]></p>\n"
  }

  "example 602" in {
    test( "foo <a href=\"&ouml;\">\n" ) shouldBe "<p>foo <a href=\"&ouml;\"></p>\n"
  }

  "example 603" in {
    test( "foo <a href=\"\\*\">\n" ) shouldBe "<p>foo <a href=\"\\*\"></p>\n"
  }

  "example 604" in {
    test( "<a href=\"\\\"\">\n" ) shouldBe "<p>&lt;a href=&quot;&quot;&quot;&gt;</p>\n"
  }

}
