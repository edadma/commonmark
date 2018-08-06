//@
package xyz.hyperreal.commonmark


object IndentedBlockType extends BlockType {

  val indentedRegex = """    (.+)"""r

  override def start( from: Int, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser ) =
    if (indentedRegex.pattern.matcher( s.head.substring(from) ).matches && nonBlank(from, s))
      Some( new IndentedBlock )
    else
      None

}

class IndentedBlock extends TextLeafBlock {

  val name = "indented"

  def accept( from: Int, stream: Stream[String] ) = {
    println( s"indented: $from, $stream")
    if (IndentedBlockType.indentedRegex.pattern.matcher( stream.head.substring(from) ).matches)
      Some( from + 4 )
    else
      None
  }

}