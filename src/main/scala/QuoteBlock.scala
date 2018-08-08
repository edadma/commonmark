package xyz.hyperreal.commonmark


object QuoteBlockType extends BlockType {

  def start( from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser ):
    Option[(Block, Int, String)] =
    if (text startsWith "> ")
      Some( (new QuoteBlock, from + 2, text substring 2) )
    else if (text == ">")
      Some( (new QuoteBlock, from + 1, "") )
    else
      None

}

class QuoteBlock extends ContainerBlock {

  val name = "quote"

  def accept( from: Int, text: String, stream: Stream[String]) : Option[(Int, String)] =
    if (text startsWith "> ")
      Some( (from + 2, text substring 2) )
    else if (text == ">")
      Some( (from + 1, "") )
    else
      None

}