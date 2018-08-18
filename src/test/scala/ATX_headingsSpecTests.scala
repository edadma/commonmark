package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class ATX_headingsSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 32" in {
    test( "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo\n" ) shouldBe "<h1>foo</h1>\n<h2>foo</h2>\n<h3>foo</h3>\n<h4>foo</h4>\n<h5>foo</h5>\n<h6>foo</h6>\n"
  }

  "example 33" in {
    test( "####### foo\n" ) shouldBe "<p>####### foo</p>\n"
  }

  "example 34" in {
    test( "#5 bolt\n\n#hashtag\n" ) shouldBe "<p>#5 bolt</p>\n<p>#hashtag</p>\n"
  }

  "example 35" in {
    test( "\\## foo\n" ) shouldBe "<p>## foo</p>\n"
  }

//  "example 36" in {//todo: em
//    test( "# foo *bar* \\*baz\\*\n" ) shouldBe "<h1>foo <em>bar</em> *baz*</h1>\n"
//  }

  "example 37" in {
    test( "#                  foo                     \n" ) shouldBe "<h1>foo</h1>\n"
  }

  "example 38" in {
    test( " ### foo\n  ## foo\n   # foo\n" ) shouldBe "<h3>foo</h3>\n<h2>foo</h2>\n<h1>foo</h1>\n"
  }

  "example 39" in {
    test( "    # foo\n" ) shouldBe "<pre><code># foo\n</code></pre>\n"
  }

  "example 40" in {
    test( "foo\n    # bar\n" ) shouldBe "<p>foo\n# bar</p>\n"
  }

  "example 41" in {
    test( "## foo ##\n  ###   bar    ###\n" ) shouldBe "<h2>foo</h2>\n<h3>bar</h3>\n"
  }

  "example 42" in {
    test( "# foo ##################################\n##### foo ##\n" ) shouldBe "<h1>foo</h1>\n<h5>foo</h5>\n"
  }

  "example 43" in {
    test( "### foo ###     \n" ) shouldBe "<h3>foo</h3>\n"
  }

  "example 44" in {
    test( "### foo ### b\n" ) shouldBe "<h3>foo ### b</h3>\n"
  }

  "example 45" in {
    test( "# foo#\n" ) shouldBe "<h1>foo#</h1>\n"
  }

  "example 46" in {
    test( "### foo \\###\n## foo #\\##\n# foo \\#\n" ) shouldBe "<h3>foo ###</h3>\n<h2>foo ###</h2>\n<h1>foo #</h1>\n"
  }

  "example 47" in {
    test( "****\n## foo\n****\n" ) shouldBe "<hr />\n<h2>foo</h2>\n<hr />\n"
  }

  "example 48" in {
    test( "Foo bar\n# baz\nBar foo\n" ) shouldBe "<p>Foo bar</p>\n<h1>baz</h1>\n<p>Bar foo</p>\n"
  }

  "example 49" in {
    test( "## \n#\n### ###\n" ) shouldBe "<h2></h2>\n<h1></h1>\n<h3></h3>\n"
  }

}
