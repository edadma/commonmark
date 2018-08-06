package xyz.hyperreal.commonmark


object ReferenceBlockType extends BlockType {

  val linkRegex = """[ ]{0,3}\[([^\]]+)\]:\s*.+?\s*(?:"(.*?)")"""r

  override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser): Option[(Block, Int, String)] =
    if (isBlank( from, s ))
      Some( (ReferenceBlock, from, text) )
    else
      None

}

object ReferenceBlock extends SpecialLeafBlock {  // this is an object and not a class for efficiency

  val name = "reference"

  keep = false

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)] =
    if (isBlank( from, stream))
      Some( (from, text) )
    else
      None

}