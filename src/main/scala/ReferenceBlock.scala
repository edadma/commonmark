package xyz.hyperreal.commonmark


object ReferenceBlockType extends BlockType {

  val linkRegex = """[ ]{0,3}\[([^\]]+)\]:\s*.+?\s*(?:"(.*?)")"""r

  override def start(from: Int, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser): Option[Block] =
    if (isBlank( from, s ))
      Some( ReferenceBlock )
    else
      None

}

object ReferenceBlock extends SpecialLeafBlock {  // this is an object and not a class for efficiency

  val name = "reference"

  keep = false

  def accept( from: Int, stream: Stream[String] ) =
    if (isBlank( from, stream))
      Some( from )
    else
      None

}