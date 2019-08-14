package xyz.hyperreal.commonmark

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class InlinesSpecTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

  "example 288" in {
    test( "`hi`lo`\n" ) shouldBe "<p><code>hi</code>lo`</p>\n"
  }

}
