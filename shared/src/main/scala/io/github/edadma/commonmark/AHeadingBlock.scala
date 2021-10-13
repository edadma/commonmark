package io.github.edadma.commonmark

object AHeadingBlockType extends BlockType {

  val aHeadingRegex =
    """[ \ue000]{0,3}(#{1,6})[\s\ue000]*?(?:([ \ue000][^#]+?)?(?:[\s\ue000]+#+)?[\s\ue000]*|[ \ue000](.*))""".r

  override def start(from: Int,
                     text: String,
                     s: LazyList[String],
                     prev: ContainerBlock,
                     parser: CommonMarkParser,
                     doc: DocumentBlock): Option[(Block, Int, String)] =
    text match {
      case aHeadingRegex(level, heading, rest) =>
        val h =
          if (heading eq null)
            if (rest eq null)
              ""
            else
              rest.trim
          else
            heading.trim

        Some((new AHeadingBlock(level.length, h), level.length, h))
      case _ => None
    }

}

class AHeadingBlock(val level: Int, val heading: String) extends SimpleLeafBlock {

  val name = "aheading"

  def accept(from: Int, text: String, s: LazyList[String]): Option[(Int, String)] = None

  override def toString: String = super.toString + s"""[$level, "$heading"]"""
}
