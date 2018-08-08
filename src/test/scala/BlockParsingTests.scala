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
    test(
      """
        |wow
        |> poiu
        |> ====
        |> asdf
        |>     zxcv
        |>
        |>     qewr
        |>
        |>     lkjh
        |> - --
        |wee
      """.stripMargin
    ) shouldBe "document[*blank, paragraph[wow], quote[*paragraph[poiu], sheading[1, poiu], paragraph[asdf\n    zxcv], *blank, indented[qewr\n\nlkjh], break], paragraph[wee], *blank]"
  }

  "fenced code" in {
    test(
      """
        |```
        |asdf
        |
        |qwer
        |```
        |eryt
      """.stripMargin
    ) shouldBe "document[*blank, fenced[asdf\n\nqwer], paragraph[eryt], *blank]"
  }

  "indented code" in {
    test(
      """
        |    zxcv
        |asdf
      """.stripMargin
    ) shouldBe "document[*blank, indented[zxcv], paragraph[asdf], *blank]"
  }

  "setext headings" in {
    test(
      """
        |poiu
        |====
        |asdf
        |    zxcv
        |
        |    qewr
        |
        |    lkjh
        |- --
      """.stripMargin
    ) shouldBe "document[*blank, *paragraph[poiu], sheading[1, poiu], paragraph[asdf\n    zxcv], *blank, indented[qewr\n\nlkjh], break, *blank]"
  }

}