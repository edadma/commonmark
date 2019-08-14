package xyz.hyperreal.commonmark

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class BlockParsingTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Testing {

	"paragraphs" in {
		testBlockParsing(
      """
			  |asdf
		  """.stripMargin
    ) shouldBe "document[*blank, paragraph[asdf], *blank],Map()"
	}

  "blockquotes" in {
    testBlockParsing(
      """
        |>     asdf
        |> qwer
        |zxcv
      """.stripMargin
    ) shouldBe "document[*blank, quote[indented[asdf], paragraph[qwer\nzxcv]], *blank],Map()"
    testBlockParsing(
      """
        |> asdf
        |> > qwer
        |zxcv
      """.stripMargin
    ) shouldBe "document[*blank, quote[paragraph[asdf], quote[paragraph[qwer\nzxcv]]], *blank],Map()"
    testBlockParsing(
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
    ) shouldBe "document[*blank, paragraph[wow], quote[*paragraph[poiu], sheading[1, poiu], paragraph[asdf\nzxcv], *blank, indented[qewr\n\nlkjh], break], paragraph[wee], *blank],Map()"
  }

  "fenced code" in {
    testBlockParsing(
      """
        |```
        |asdf
        |
        |qwer
        |```
        |eryt
      """.stripMargin
    ) shouldBe "document[*blank, fenced[asdf\n\nqwer], paragraph[eryt], *blank],Map()"
  }

  "indented code" in {
    testBlockParsing(
      """
        |    zxcv
        |asdf
      """.stripMargin
    ) shouldBe "document[*blank, indented[zxcv], paragraph[asdf], *blank],Map()"
  }

  "setext headings" in {
    testBlockParsing(
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
    ) shouldBe "document[*blank, *paragraph[poiu], sheading[1, poiu], paragraph[asdf\nzxcv], *blank, indented[qewr\n\nlkjh], break, *blank],Map()"
  }

}