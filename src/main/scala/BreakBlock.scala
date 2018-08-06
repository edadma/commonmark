package xyz.hyperreal.commonmark


object BreakBlockType extends BlockType {

  val breakRegex = """[ ]{0,3}([-_*])\s*(?:\1\s*){2,}"""r

  protected override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser): Option[(Block, Int, String)] =
    if (breakRegex.pattern.matcher( s.head.subSequence(from, s.head.length) ).matches)
      Some( (new BreakBlock, from) )
    else
      None

}

class BreakBlock extends SimpleLeafBlock {

  val name = "break"

  def accept(from: Int, text: String, s: Stream[String]): Option[(Int, String)] =
    if (BreakBlockType.breakRegex.pattern.matcher( s.head.subSequence(from, s.head.length) ).matches)
      Some( from )
    else
      None

}