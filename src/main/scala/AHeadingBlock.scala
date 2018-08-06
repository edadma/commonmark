package xyz.hyperreal.commonmark


object AHeadingBlockType extends BlockType {

  val aHeadingRegex = """[ ]{0,3}(#{1,6})(\s+)([^#\n]*?)(?:\s+#+)?"""r

  protected override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser): Option[(Block, Int, String)] =
    s.head substring from match {
      case aHeadingRegex( level, space, heading ) => Some( (new AHeadingBlock(level.length, heading), level.length + space.length, heading) )
      case _ => None
    }

}

class AHeadingBlock( level: Int, heading: String ) extends SimpleLeafBlock {

  val name = "aheading"

  def accept(from: Int, text: String, s: Stream[String]): Option[(Int, String)] =
    if (AHeadingBlockType.aHeadingRegex.pattern.matcher( s.head.subSequence(from, s.head.length) ).matches)
      Some( from )
    else
      None

  override def toString: String = super.toString + s"""[$level, "$heading"]"""
}