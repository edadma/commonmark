package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class BlockParsingTests extends FreeSpec with PropertyChecks with Matchers with Testing {
	
	"paragraphs" in {
		test(
      """
			  |asdf
		  """.stripMargin
    ) shouldBe "document[*blank, paragraph[asdf], *blank]"

	}

  "blockquotes" in {
    test(
      """
        |>     asdf
        |> qwer
        |zxcv
      """.stripMargin
    ) shouldBe "document[*blank, quote[indented[asdf], paragraph[qwer]], paragraph[zxcv], *blank]"

  }

}