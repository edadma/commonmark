package xyz.hyperreal.commonmark

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class PrecedenceSpecTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

  "example 12" in {
    test( "- `one\n- two`\n" ) shouldBe "<ul>\n<li>`one</li>\n<li>two`</li>\n</ul>\n"
  }

}
