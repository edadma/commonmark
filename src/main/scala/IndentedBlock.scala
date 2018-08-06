//@
package xyz.hyperreal.commonmark


object IndentedBlockType extends BlockType {

  val indentedRegex = """    (.+)"""r

  override def start( from: Int, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser ) =
    if (indentedRegex.pattern.matcher( s.head.substring(from) ).matches && nonBlank(from, s) && !prev.open.exists( _.isInstanceOf[ParagraphBlock]))
      Some( (new IndentedBlock, from + 4) )
    else
      None

}

class IndentedBlock extends TextLeafBlock {

  val name = "indented"

  def accept( from: Int, stream: Stream[String] ) = {
    if (IndentedBlockType.indentedRegex.pattern.matcher( stream.head.substring(from) ).matches)
      Some( from + 4 )
    else
      None
  }

}