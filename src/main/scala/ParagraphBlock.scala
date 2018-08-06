package xyz.hyperreal.commonmark


object ParagraphBlockType extends BlockType {

  protected override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser): Option[(Block, Int, String)] =
    if (nonBlank( from, s )/* && (s.head.substring(from).length < 4 || !s.head.substring(from)(3).isWhitespace)*/)
      Some( (new ParagraphBlock, from, text) )
    else
      None

}

class ParagraphBlock extends TextLeafBlock {

  val name = "paragraph"

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)] =
    if (nonBlank( from, stream))
      Some( (from, text) )
    else
      None

}