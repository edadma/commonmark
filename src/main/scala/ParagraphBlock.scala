package xyz.hyperreal.commonmark


object ParagraphBlockType extends BlockType {

  override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock): Option[(Block, Int, String)] =
    if (nonBlank( text )/* && !prev.open.exists( _.isInstanceOf[IndentedBlock])*/)
      Some( (new ParagraphBlock, from, text) )
    else
      None

}

class ParagraphBlock extends TextLeafBlock {

  val name = "paragraph"

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)] =
    if (nonBlank( text ))
      Some( (from, text) )
    else
      None

}