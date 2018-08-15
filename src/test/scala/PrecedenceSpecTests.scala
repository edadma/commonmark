package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class PrecedenceSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 12" in {
    test( "- `one\n- two`\n" ) shouldBe "<ul>\n<li>`one</li>\n<li>two`</li>\n</ul>\n"
  }

}
