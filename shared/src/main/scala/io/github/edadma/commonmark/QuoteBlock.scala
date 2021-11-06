package io.github.edadma.commonmark

import scala.util.matching.Regex

object QuoteBlockType extends BlockType {

  val quoteRegex: Regex = s"""([ $tab]{0,3}>[ $tab]?)(.*)""".r

  def accept(from: Int, text: String): Option[(Int, String)] =
    text match {
      case quoteRegex(marker, newtext) => Some((from + marker.length, newtext))
      case _                           => None
    }

  def start(from: Int,
            text: String,
            s: LazyList[String],
            prev: ContainerBlock,
            parser: CommonMarkParser,
            doc: DocumentBlock): Option[(Block, Int, String)] =
    accept(from, text) match {
      case Some((marker, newtext)) => Some((new QuoteBlock, from + marker, newtext))
      case _                       => None
    }

}

class QuoteBlock extends ContainerBlock {

  val name = "quote"

  def accept(from: Int, text: String, stream: LazyList[String]): Option[(Int, String)] =
    QuoteBlockType.accept(from, text)

}
