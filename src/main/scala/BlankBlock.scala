package xyz.hyperreal.commonmark


object BlankBlockType extends BlockType {

  override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser): Option[(Block, Int, String)] =
    if (isBlank( from, s ))
      Some( (BlankBlock, from, text) )
    else
      None

}

object BlankBlock extends SpecialLeafBlock {  // this is an object and not a class for efficiency

  val name = "blank"

  keep = false

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)] =
    if (isBlank( from, stream))
      Some( (from, text) )
    else
      None

}