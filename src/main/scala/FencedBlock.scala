package xyz.hyperreal.commonmark


object FencedBlockType extends BlockType {

  val startFenceRegex = """[ ]{0,3}(`{3,}|~{3,})([^`\n]*)"""r
  val endFenceRegex = """[ ]{0,3}(`{3,}|~{3,})\s*"""r

  override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock): Option[(Block, Int, String)] =
    text match {
      case startFenceRegex( fence, info ) => Some( (new FencedBlock(fence, info.trim), from, "") )
      case _ => None
    }

  def end( startfence: String, text: String ) =
    text match {
      case endFenceRegex( fence ) if fence(0) == startfence(0) && fence.length >= startfence.length => true
      case _ => false
    }

}

class FencedBlock( fence: String, val info: String ) extends TextLeafBlock {

  val name = "fenced"
  var start = false
  var end = false

  override val isInterruptible = false

  def accept( from: Int, text: String, stream: Stream[String] ): Option[(Int, String)] =
    if (!start) {
      start = true
      Some( (from, text) )
    } else if (end) {
      None
    } else {
      if (FencedBlockType.end( fence, text ))
        end = true

      Some( (from, text) )
    }

  override def append( from: Int, text: String, stream: Stream[String] ): Unit =
    if (start && !end)
      super.append( from, text, stream )

}