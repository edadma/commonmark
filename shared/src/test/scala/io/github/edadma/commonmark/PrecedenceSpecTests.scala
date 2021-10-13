package io.github.edadma.commonmark

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PrecedenceSpecTests extends AnyFreeSpec with Matchers with Testing {

  "example 42" in {
    test( "- `one\n- two`\n" ) shouldBe "<ul>\n<li>`one</li>\n<li>two`</li>\n</ul>\n"
  }

}
