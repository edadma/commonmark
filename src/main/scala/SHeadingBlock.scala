package xyz.hyperreal.commonmark


object SHeadingBlockType extends BlockType {

  val sHeadingRegex = """[ ]{0,3}(-+|=+)\s*"""r

  override def start( from: Int, s: Stream[String], prev: ContainerBlock ) = {
    s.head substring from match {
      case sHeadingRegex(level) =>
        Some(new SHeadingBlock(level.length, "asdf"))
      case _ => None
    }
  }

}

class SHeadingBlock( level: Int, heading: String ) extends SimpleLeafBlock {

  val name = "aheading"

  def accept( from: Int, s: Stream[String] ) =
    if (SHeadingBlockType.sHeadingRegex.pattern.matcher( s.head.subSequence(from, s.head.length) ).matches)
      Some( from )
    else
      None

  override def toString: String = super.toString + s"""[$level, "$heading"]"""
}