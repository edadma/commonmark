package xyz.hyperreal.commonmark


object BlankBlockType extends BlockType {

  override def start( from: Int, s: Stream[String] ) =
    if (isBlank( from, s ))
      Some( new BlankBlock )
    else
      None

}

class BlankBlock extends SpecialLeafBlock {

  val name = "blank"

  def accept( from: Int, stream: Stream[String] ) =
    if (isBlank( from, stream))
      Some( from )
    else
      None

}