package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ATX_headingsSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 62" in {
    test( "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo\n" ) shouldBe "<h1>foo</h1>\n<h2>foo</h2>\n<h3>foo</h3>\n<h4>foo</h4>\n<h5>foo</h5>\n<h6>foo</h6>\n"
  }

  "example 63" in {
    test( "####### foo\n" ) shouldBe "<p>####### foo</p>\n"
  }

  "example 64" in {
    test( "#5 bolt\n\n#hashtag\n" ) shouldBe "<p>#5 bolt</p>\n<p>#hashtag</p>\n"
  }

  "example 65" in {
    test( "\\## foo\n" ) shouldBe "<p>## foo</p>\n"
  }

  "example 66" in {
    test( "# foo *bar* \\*baz\\*\n" ) shouldBe "<h1>foo <em>bar</em> *baz*</h1>\n"
  }

  "example 67" in {
    test( "#                  foo                     \n" ) shouldBe "<h1>foo</h1>\n"
  }

  "example 68" in {
    test( " ### foo\n  ## foo\n   # foo\n" ) shouldBe "<h3>foo</h3>\n<h2>foo</h2>\n<h1>foo</h1>\n"
  }

  "example 69" in {
    test( "    # foo\n" ) shouldBe "<pre><code># foo\n</code></pre>\n"
  }

  "example 70" in {
    test( "foo\n    # bar\n" ) shouldBe "<p>foo\n# bar</p>\n"
  }

  "example 71" in {
    test( "## foo ##\n  ###   bar    ###\n" ) shouldBe "<h2>foo</h2>\n<h3>bar</h3>\n"
  }

  "example 72" in {
    test( "# foo ##################################\n##### foo ##\n" ) shouldBe "<h1>foo</h1>\n<h5>foo</h5>\n"
  }

  "example 73" in {
    test( "### foo ###     \n" ) shouldBe "<h3>foo</h3>\n"
  }

  "example 74" in {
    test( "### foo ### b\n" ) shouldBe "<h3>foo ### b</h3>\n"
  }

  "example 75" in {
    test( "# foo#\n" ) shouldBe "<h1>foo#</h1>\n"
  }

  "example 76" in {
    test( "### foo \\###\n## foo #\\##\n# foo \\#\n" ) shouldBe "<h3>foo ###</h3>\n<h2>foo ###</h2>\n<h1>foo #</h1>\n"
  }

  "example 77" in {
    test( "****\n## foo\n****\n" ) shouldBe "<hr />\n<h2>foo</h2>\n<hr />\n"
  }

  "example 78" in {
    test( "Foo bar\n# baz\nBar foo\n" ) shouldBe "<p>Foo bar</p>\n<h1>baz</h1>\n<p>Bar foo</p>\n"
  }

  "example 79" in {
    test( "## \n#\n### ###\n" ) shouldBe "<h2></h2>\n<h1></h1>\n<h3></h3>\n"
  }

}
