package io.github.edadma.commonmark

import scala.util.matching.Regex

object FencedBlockType extends BlockType {

  val startFenceRegex: Regex = """([ tab]{0,3})(`{3,}|~{3,})\s*([^\s]*)(.*)""".t.r
  val endFenceRegex: Regex = """[ tab]{0,3}(`{3,}|~{3,})\s*""".t.r

  override def start(from: Int,
                     text: String,
                     s: LazyList[String],
                     prev: ContainerBlock,
                     parser: CommonMarkParser,
                     doc: DocumentBlock): Option[(Block, Int, String)] =
    text match {
      case startFenceRegex(indent, fence, info, rest)
          if fence.head == '~' || !info.contains('`') && !rest.contains('`') =>
        Some((new FencedBlock(indent, fence, info.trim), from, ""))
      case _ => None
    }

  def end(startfence: String, text: String): Boolean =
    text match {
      case endFenceRegex(fence) if fence(0) == startfence(0) && fence.length >= startfence.length => true
      case _                                                                                      => false
    }

}

class FencedBlock(indent: String, fence: String, val info: String) extends TextLeafBlock {

  val name = "fenced"
  var start = false
  var end = false

  override val isInterruptible = false

  def accept(from: Int, text: String, stream: LazyList[String]): Option[(Int, String)] =
    if (end)
      None
    else {
      if (FencedBlockType.end(fence, text))
        end = true

      Some((from, text))
    }

  override def append(from: Int, text: String, stream: LazyList[String]): Unit = {
    if (start && !end) {
      val t =
        if (text startsWith indent)
          text drop indent.length
        else
          text dropWhile (_ == ' ')

      super.append(from, t, stream)
    }

    start = true
  }

}
