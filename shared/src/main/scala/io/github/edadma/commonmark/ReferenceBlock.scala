package io.github.edadma.commonmark

import scala.util.matching.Regex

object ReferenceBlockType extends BlockType {

  val linkRegex: Regex = """[ tab]{0,3}\[([^]]+)]:\s*(.+?)\s*(?:"(.*?)")?""".t.r

  override def start(from: Int,
                     text: String,
                     s: LazyList[String],
                     prev: ContainerBlock,
                     parser: CommonMarkParser,
                     doc: DocumentBlock): Option[(Block, Int, String)] =
    text match {
      case linkRegex(ref, url, title) =>
        parser.refs(ref) = parser.LinkInfo(url, if (title eq null) None else Some(title))
        Some((ReferenceBlock, from, text))
      case _ => None
    }

}

object ReferenceBlock extends SimpleLeafBlock {

  val name = "reference"

  keep = false

  def accept(from: Int, text: String, stream: LazyList[String]): Option[(Int, String)] = Some((from, text))

}
