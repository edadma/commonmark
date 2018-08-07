//@
package xyz.hyperreal.commonmark


object IndentedBlockType extends BlockType {

  val indentedRegex = """    (.+)"""r

  override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser): Option[(Block, Int, String)] =
    if (indentedRegex.pattern.matcher( text ).matches && nonBlank( text ) && !prev.open.exists( _.isInstanceOf[ParagraphBlock]))
      Some( (new IndentedBlock, from + 4, text substring 4) )
    else
      None

}

class IndentedBlock extends TextLeafBlock {

  val name = "indented"

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)] = {
    if (IndentedBlockType.indentedRegex.pattern.matcher( stream.head.substring(from) ).matches || isBlank( text ))
      Some( (from + 4, if (text.length < 4) "" else text.substring( 4 )) )
    else
      None
  }

}