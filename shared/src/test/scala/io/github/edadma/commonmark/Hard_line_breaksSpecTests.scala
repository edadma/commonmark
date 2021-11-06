package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Hard_line_breaksSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 633" in {
    test( "foo  \nbaz\n" ) shouldBe "<p>foo<br />\nbaz</p>\n"
  }

  "example 634" in {
    test( "foo\\\nbaz\n" ) shouldBe "<p>foo<br />\nbaz</p>\n"
  }

  "example 635" in {
    test( "foo       \nbaz\n" ) shouldBe "<p>foo<br />\nbaz</p>\n"
  }

  "example 636" in {
    test( "foo  \n     bar\n" ) shouldBe "<p>foo<br />\nbar</p>\n"
  }

  "example 637" in {
    test( "foo\\\n     bar\n" ) shouldBe "<p>foo<br />\nbar</p>\n"
  }

  "example 638" in {
    test( "*foo  \nbar*\n" ) shouldBe "<p><em>foo<br />\nbar</em></p>\n"
  }

  "example 639" in {
    test( "*foo\\\nbar*\n" ) shouldBe "<p><em>foo<br />\nbar</em></p>\n"
  }

  "example 640" in {
    test( "`code  \nspan`\n" ) shouldBe "<p><code>code   span</code></p>\n"
  }

  "example 641" in {
    test( "`code\\\nspan`\n" ) shouldBe "<p><code>code\\ span</code></p>\n"
  }

  "example 642" in {
    test( "<a href=\"foo  \nbar\">\n" ) shouldBe "<p><a href=\"foo  \nbar\"></p>\n"
  }

  "example 643" in {
    test( "<a href=\"foo\\\nbar\">\n" ) shouldBe "<p><a href=\"foo\\\nbar\"></p>\n"
  }

  "example 644" in {
    test( "foo\\\n" ) shouldBe "<p>foo\\</p>\n"
  }

  "example 645" in {
    test( "foo  \n" ) shouldBe "<p>foo</p>\n"
  }

  "example 646" in {
    test( "### foo\\\n" ) shouldBe "<h3>foo\\</h3>\n"
  }

  "example 647" in {
    test( "### foo  \n" ) shouldBe "<h3>foo</h3>\n"
  }

}
