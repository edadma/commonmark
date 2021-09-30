package io.github.edadma.commonmark

object QuoteBlockType extends BlockType {

  val quoteRegex = """([ ]{0,3}> ?)(.*)""" r

  def accept(from: Int, text: String) =
    text match {
      case quoteRegex(marker, newtext) => Some((from + marker.length, newtext))
      case _                           => None
    }

  def start(from: Int,
            text: String,
            s: Stream[String],
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

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)] =
    QuoteBlockType.accept(from, text)

}
