package xyz.hyperreal.commonmark


object ParagraphBlockType extends BlockType {

  override def start( from: Int, s: Stream[String] ) =
    if (nonBlank( from, s ))
      Some( new ParagraphBlock )
    else
      None

}

class ParagraphBlock extends TextLeafBlock {

  val name = "paragraph"

  def accept( from: Int, stream: Stream[String] ) =
    if (nonBlank( from, stream))
      Some( from )
    else
      None

}