package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class AutolinksSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 565" in {
    test( "<http://foo.bar.baz>\n" ) shouldBe "<p><a href=\"http://foo.bar.baz\">http://foo.bar.baz</a></p>\n"
  }

  "example 566" in {
    test( "<http://foo.bar.baz/test?q=hello&id=22&boolean>\n" ) shouldBe "<p><a href=\"http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean\">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>\n"
  }

  "example 567" in {
    test( "<irc://foo.bar:2233/baz>\n" ) shouldBe "<p><a href=\"irc://foo.bar:2233/baz\">irc://foo.bar:2233/baz</a></p>\n"
  }

  "example 568" in {
    test( "<MAILTO:FOO@BAR.BAZ>\n" ) shouldBe "<p><a href=\"MAILTO:FOO@BAR.BAZ\">MAILTO:FOO@BAR.BAZ</a></p>\n"
  }

  "example 569" in {
    test( "<a+b+c:d>\n" ) shouldBe "<p><a href=\"a+b+c:d\">a+b+c:d</a></p>\n"
  }

  "example 570" in {
    test( "<made-up-scheme://foo,bar>\n" ) shouldBe "<p><a href=\"made-up-scheme://foo,bar\">made-up-scheme://foo,bar</a></p>\n"
  }

  "example 571" in {
    test( "<http://../>\n" ) shouldBe "<p><a href=\"http://../\">http://../</a></p>\n"
  }

  "example 572" in {
    test( "<localhost:5001/foo>\n" ) shouldBe "<p><a href=\"localhost:5001/foo\">localhost:5001/foo</a></p>\n"
  }

  "example 573" in {
    test( "<http://foo.bar/baz bim>\n" ) shouldBe "<p>&lt;http://foo.bar/baz bim&gt;</p>\n"
  }

  "example 574" in {
    test( "<http://example.com/\\[\\>\n" ) shouldBe "<p><a href=\"http://example.com/%5C%5B%5C\">http://example.com/\\[\\</a></p>\n"
  }

  "example 575" in {
    test( "<foo@bar.example.com>\n" ) shouldBe "<p><a href=\"mailto:foo@bar.example.com\">foo@bar.example.com</a></p>\n"
  }

  "example 576" in {
    test( "<foo+special@Bar.baz-bar0.com>\n" ) shouldBe "<p><a href=\"mailto:foo+special@Bar.baz-bar0.com\">foo+special@Bar.baz-bar0.com</a></p>\n"
  }

  "example 577" in {
    test( "<foo\\+@bar.example.com>\n" ) shouldBe "<p>&lt;foo+@bar.example.com&gt;</p>\n"
  }

  "example 578" in {
    test( "<>\n" ) shouldBe "<p>&lt;&gt;</p>\n"
  }

  "example 579" in {
    test( "< http://foo.bar >\n" ) shouldBe "<p>&lt; http://foo.bar &gt;</p>\n"
  }

  "example 580" in {
    test( "<m:abc>\n" ) shouldBe "<p>&lt;m:abc&gt;</p>\n"
  }

  "example 581" in {
    test( "<foo.bar.baz>\n" ) shouldBe "<p>&lt;foo.bar.baz&gt;</p>\n"
  }

  "example 582" in {
    test( "http://example.com\n" ) shouldBe "<p>http://example.com</p>\n"
  }

  "example 583" in {
    test( "foo@bar.example.com\n" ) shouldBe "<p>foo@bar.example.com</p>\n"
  }

}
