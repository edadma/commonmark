//@
package io.github.edadma.commonmark

object IndentedBlockType extends BlockType {

  val indentedRegex = """[ \ue000]{4}(.+)""".r

  override def start(from: Int,
                     text: String,
                     s: LazyList[String],
                     prev: ContainerBlock,
                     parser: CommonMarkParser,
                     doc: DocumentBlock): Option[(Block, Int, String)] =
    if (indentedRegex.pattern.matcher(text).matches && nonBlank(text) && !prev.open.exists(
          _.isInstanceOf[ParagraphBlock]))
      Some((new IndentedBlock, from + 4, text substring 4))
    else
      None

}

class IndentedBlock extends TextLeafBlock {

  val name = "indented"

  override val isInterruptible = false

  def accept(from: Int, text: String, stream: LazyList[String]): Option[(Int, String)] = {
    if (IndentedBlockType.indentedRegex.pattern.matcher(stream.head.substring(from)).matches || isBlank(text))
      Some((from + 4, if (text.length < 4) "" else text.substring(4)))
    else
      None
  }

}
