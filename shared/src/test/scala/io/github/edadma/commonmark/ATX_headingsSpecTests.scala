package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ATX_headingsSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 32.0" in {
    test( "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo\n" ) shouldBe "<h1>foo</h1>\n<h2>foo</h2>\n<h3>foo</h3>\n<h4>foo</h4>\n<h5>foo</h5>\n<h6>foo</h6>\n"
  }

  "example 33.0" in {
    test( "####### foo\n" ) shouldBe "<p>####### foo</p>\n"
  }

  "example 34.0" in {
    test( "#5 bolt\n\n#hashtag\n" ) shouldBe "<p>#5 bolt</p>\n<p>#hashtag</p>\n"
  }

  "example 35.0" in {
    test( "\\## foo\n" ) shouldBe "<p>## foo</p>\n"
  }

  "example 36.0" in {
    test( "# foo *bar* \\*baz\\*\n" ) shouldBe "<h1>foo <em>bar</em> *baz*</h1>\n"
  }

  "example 37.0" in {
    test( "#                  foo                     \n" ) shouldBe "<h1>foo</h1>\n"
  }

  "example 38.0" in {
    test( " ### foo\n  ## foo\n   # foo\n" ) shouldBe "<h3>foo</h3>\n<h2>foo</h2>\n<h1>foo</h1>\n"
  }

  "example 39.0" in {
    test( "    # foo\n" ) shouldBe "<pre><code># foo\n</code></pre>\n"
  }

  "example 40.0" in {
    test( "foo\n    # bar\n" ) shouldBe "<p>foo\n# bar</p>\n"
  }

  "example 41.0" in {
    test( "## foo ##\n  ###   bar    ###\n" ) shouldBe "<h2>foo</h2>\n<h3>bar</h3>\n"
  }

  "example 42.0" in {
    test( "# foo ##################################\n##### foo ##\n" ) shouldBe "<h1>foo</h1>\n<h5>foo</h5>\n"
  }

  "example 43.0" in {
    test( "### foo ###     \n" ) shouldBe "<h3>foo</h3>\n"
  }

  "example 44.0" in {
    test( "### foo ### b\n" ) shouldBe "<h3>foo ### b</h3>\n"
  }

  "example 45.0" in {
    test( "# foo#\n" ) shouldBe "<h1>foo#</h1>\n"
  }

  "example 46.0" in {
    test( "### foo \\###\n## foo #\\##\n# foo \\#\n" ) shouldBe "<h3>foo ###</h3>\n<h2>foo ###</h2>\n<h1>foo #</h1>\n"
  }

  "example 47.0" in {
    test( "****\n## foo\n****\n" ) shouldBe "<hr />\n<h2>foo</h2>\n<hr />\n"
  }

  "example 48.0" in {
    test( "Foo bar\n# baz\nBar foo\n" ) shouldBe "<p>Foo bar</p>\n<h1>baz</h1>\n<p>Bar foo</p>\n"
  }

  "example 49.0" in {
    test( "## \n#\n### ###\n" ) shouldBe "<h2></h2>\n<h1></h1>\n<h3></h3>\n"
  }

}
