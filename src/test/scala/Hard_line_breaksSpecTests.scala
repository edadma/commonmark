package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class Hard_line_breaksSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 605" in {
    test( "foo  \nbaz\n" ) shouldBe "<p>foo<br />\nbaz</p>\n"
  }

  "example 606" in {
    test( "foo\\\nbaz\n" ) shouldBe "<p>foo<br />\nbaz</p>\n"
  }

  "example 607" in {
    test( "foo       \nbaz\n" ) shouldBe "<p>foo<br />\nbaz</p>\n"
  }

  "example 608" in {
    test( "foo  \n     bar\n" ) shouldBe "<p>foo<br />\nbar</p>\n"
  }

  "example 609" in {
    test( "foo\\\n     bar\n" ) shouldBe "<p>foo<br />\nbar</p>\n"
  }

//  "example 610" in {//todo: em
//    test( "*foo  \nbar*\n" ) shouldBe "<p><em>foo<br />\nbar</em></p>\n"
//  }

//  "example 611" in {//todo: em
//    test( "*foo\\\nbar*\n" ) shouldBe "<p><em>foo<br />\nbar</em></p>\n"
//  }

  "example 612" in {
    test( "`code  \nspan`\n" ) shouldBe "<p><code>code span</code></p>\n"
  }

  "example 613" in {
    test( "`code\\\nspan`\n" ) shouldBe "<p><code>code\\ span</code></p>\n"
  }

//  "example 614" in {//todo: html
//    test( "<a href=\"foo  \nbar\">\n" ) shouldBe "<p><a href=\"foo  \nbar\"></p>\n"
//  }

//  "example 615" in {//todo: html
//    test( "<a href=\"foo\\\nbar\">\n" ) shouldBe "<p><a href=\"foo\\\nbar\"></p>\n"
//  }

  "example 616" in {
    test( "foo\\\n" ) shouldBe "<p>foo\\</p>\n"
  }

  "example 617" in {
    test( "foo  \n" ) shouldBe "<p>foo</p>\n"
  }

  "example 618" in {
    test( "### foo\\\n" ) shouldBe "<h3>foo\\</h3>\n"
  }

  "example 619" in {
    test( "### foo  \n" ) shouldBe "<h3>foo</h3>\n"
  }

}
