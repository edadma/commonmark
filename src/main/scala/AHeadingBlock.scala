package xyz.hyperreal.commonmark


object AHeadingBlockType extends BlockType {

  val aHeadingRegex = """[ ]{0,3}(#{1,6})\s+([^#\n]*?)(?:\s+#+)?"""r

  override def start( from: Int, s: Stream[String], prev: ContainerBlock ) =
    s.head substring from match {
      case aHeadingRegex( level, heading ) => Some( new AHeadingBlock(level.length, heading) )
      case _ => None
    }

}

class AHeadingBlock( level: Int, heading: String ) extends SimpleLeafBlock {

  val name = "aheading"

  def accept( from: Int, s: Stream[String] ) =
    if (AHeadingBlockType.aHeadingRegex.pattern.matcher( s.head.subSequence(from, s.head.length) ).matches)
      Some( from )
    else
      None

  override def toString: String = super.toString + s"""[$level, "$heading"]"""
}