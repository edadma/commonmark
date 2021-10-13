package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InlinesSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 327" in {
    test( "`hi`lo`\n" ) shouldBe "<p><code>hi</code>lo`</p>\n"
  }

}
