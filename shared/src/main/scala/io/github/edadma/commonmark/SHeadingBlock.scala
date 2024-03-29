package io.github.edadma.commonmark

import scala.util.matching.Regex

object SHeadingBlockType extends BlockType {

  val sHeadingRegex: Regex = """[ tab]{0,3}(-+|=+)\s*""".t.r

  override def start(from: Int,
                     text: String,
                     s: LazyList[String],
                     prev: ContainerBlock,
                     parser: CommonMarkParser,
                     doc: DocumentBlock): Option[(Block, Int, String)] = {
    text match {
      case sHeadingRegex(underline) =>
        prev.open match {
          case Some(p: ParagraphBlock) =>
            p.keep = false
            Some((new SHeadingBlock(if (underline.head == '=') 1 else 2, p.buf.toString), from, text))
          case _ => None
        }
      case _ => None
    }
  }

}

class SHeadingBlock(val level: Int, val heading: String) extends SimpleLeafBlock {

  val name = "sheading"

  def accept(from: Int, text: String, s: LazyList[String]): Option[(Int, String)] =
    if (SHeadingBlockType.sHeadingRegex.pattern.matcher(text).matches)
      Some((from, text))
    else
      None

  override def toString: String = super.toString + s"[$level, $heading]"

}
