package xyz.hyperreal.commonmark


object SHeadingBlockType extends BlockType {

  val sHeadingRegex = """[ ]{0,3}(-+|=+)\s*"""r

  override def start(from: Int, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser): Option[Block] = {
    s.head substring from match {
      case sHeadingRegex( underline ) =>
        prev.open match {
          case Some( p: ParagraphBlock ) =>
            p.keep = false
            Some( new SHeadingBlock(if (underline.head == '=') 1 else 2, p.text.toString) )
          case _ => None
        }
      case _ => None
    }
  }

}

class SHeadingBlock( level: Int, heading: String ) extends SimpleLeafBlock {

  val name = "sheading"

  def accept( from: Int, s: Stream[String] ) =
    if (SHeadingBlockType.sHeadingRegex.pattern.matcher( s.head.subSequence(from, s.head.length) ).matches)
      Some( from )
    else
      None

  override def toString: String = super.toString + s"""[$level, "$heading"]"""
}