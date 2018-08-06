package xyz.hyperreal.commonmark


object BlankBlockType extends BlockType {

  override def start( from: Int, s: Stream[String], prev: ContainerBlock ) =
    if (isBlank( from, s ))
      Some( BlankBlock )
    else
      None

}

object BlankBlock extends SpecialLeafBlock {  // this is an object and not a class for efficiency

  val name = "blank"

  keep = false

  def accept( from: Int, stream: Stream[String] ) =
    if (isBlank( from, stream))
      Some( from )
    else
      None

}