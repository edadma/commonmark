package io.github.edadma.commonmark

import org.scalatest._
import prop.PropertyChecks

class InlinesSpecTests extends FreeSpec with PropertyChecks with Matchers with Testing {

  "example 288" in {
    test("`hi`lo`\n") shouldBe "<p><code>hi</code>lo`</p>\n"
  }

}
