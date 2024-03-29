package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AutolinksSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 593" in {
    test( "<http://foo.bar.baz>\n" ) shouldBe "<p><a href=\"http://foo.bar.baz\">http://foo.bar.baz</a></p>\n"
  }

  "example 594" in {
    test( "<http://foo.bar.baz/test?q=hello&id=22&boolean>\n" ) shouldBe "<p><a href=\"http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean\">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>\n"
  }

  "example 595" in {
    test( "<irc://foo.bar:2233/baz>\n" ) shouldBe "<p><a href=\"irc://foo.bar:2233/baz\">irc://foo.bar:2233/baz</a></p>\n"
  }

  "example 596" in {
    test( "<MAILTO:FOO@BAR.BAZ>\n" ) shouldBe "<p><a href=\"MAILTO:FOO@BAR.BAZ\">MAILTO:FOO@BAR.BAZ</a></p>\n"
  }

  "example 597" in {
    test( "<a+b+c:d>\n" ) shouldBe "<p><a href=\"a+b+c:d\">a+b+c:d</a></p>\n"
  }

  "example 598" in {
    test( "<made-up-scheme://foo,bar>\n" ) shouldBe "<p><a href=\"made-up-scheme://foo,bar\">made-up-scheme://foo,bar</a></p>\n"
  }

  "example 599" in {
    test( "<http://../>\n" ) shouldBe "<p><a href=\"http://../\">http://../</a></p>\n"
  }

  "example 600" in {
    test( "<localhost:5001/foo>\n" ) shouldBe "<p><a href=\"localhost:5001/foo\">localhost:5001/foo</a></p>\n"
  }

  "example 601" in {
    test( "<http://foo.bar/baz bim>\n" ) shouldBe "<p>&lt;http://foo.bar/baz bim&gt;</p>\n"
  }

  "example 602" in {
    test( "<http://example.com/\\[\\>\n" ) shouldBe "<p><a href=\"http://example.com/%5C%5B%5C\">http://example.com/\\[\\</a></p>\n"
  }

  "example 603" in {
    test( "<foo@bar.example.com>\n" ) shouldBe "<p><a href=\"mailto:foo@bar.example.com\">foo@bar.example.com</a></p>\n"
  }

  "example 604" in {
    test( "<foo+special@Bar.baz-bar0.com>\n" ) shouldBe "<p><a href=\"mailto:foo+special@Bar.baz-bar0.com\">foo+special@Bar.baz-bar0.com</a></p>\n"
  }

  "example 605" in {
    test( "<foo\\+@bar.example.com>\n" ) shouldBe "<p>&lt;foo+@bar.example.com&gt;</p>\n"
  }

  "example 606" in {
    test( "<>\n" ) shouldBe "<p>&lt;&gt;</p>\n"
  }

  "example 607" in {
    test( "< http://foo.bar >\n" ) shouldBe "<p>&lt; http://foo.bar &gt;</p>\n"
  }

  "example 608" in {
    test( "<m:abc>\n" ) shouldBe "<p>&lt;m:abc&gt;</p>\n"
  }

  "example 609" in {
    test( "<foo.bar.baz>\n" ) shouldBe "<p>&lt;foo.bar.baz&gt;</p>\n"
  }

  "example 610" in {
    test( "http://example.com\n" ) shouldBe "<p>http://example.com</p>\n"
  }

  "example 611" in {
    test( "foo@bar.example.com\n" ) shouldBe "<p>foo@bar.example.com</p>\n"
  }

}
