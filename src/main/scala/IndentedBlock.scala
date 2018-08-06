package xyz.hyperreal.commonmark


object IndentedBlockType extends BlockType {

  override def start( from: Int, s: Stream[String], prev: ContainerBlock ) =
    if (nonBlank( from, s ))
      Some( new IndentedBlock )
    else
      None

}

class IndentedBlock extends TextLeafBlock {

  val name = "indented"

  def accept( from: Int, stream: Stream[String] ) =
    if (nonBlank( from, stream))
      Some( from )
    else
      None

}