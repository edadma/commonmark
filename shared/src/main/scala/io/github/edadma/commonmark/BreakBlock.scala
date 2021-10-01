package io.github.edadma.commonmark

object BreakBlockType extends BlockType {

  val breakRegex = """[ ]{0,3}([-_*])\s*(?:\1\s*){2,}""" r

  override def start(from: Int,
                     text: String,
                     s: LazyList[String],
                     prev: ContainerBlock,
                     parser: CommonMarkParser,
                     doc: DocumentBlock): Option[(Block, Int, String)] =
    if (breakRegex.pattern.matcher(text).matches)
      Some((new BreakBlock, from, text))
    else
      None

}

class BreakBlock extends SimpleLeafBlock {

  val name = "break"

  def accept(from: Int, text: String, s: LazyList[String]): Option[(Int, String)] =
    if (BreakBlockType.breakRegex.pattern.matcher(s.head.subSequence(from, s.head.length)).matches)
      Some((from, text))
    else
      None

}
