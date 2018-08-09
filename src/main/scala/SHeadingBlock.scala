package xyz.hyperreal.commonmark


object SHeadingBlockType extends BlockType {

  val sHeadingRegex = """[ ]{0,3}(-+|=+)\s*"""r

  override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock): Option[(Block, Int, String)] = {
    text match {
      case sHeadingRegex( underline ) =>
        prev.open match {
          case Some( p: ParagraphBlock ) =>
            p.keep = false
            Some( (new SHeadingBlock(if (underline.head == '=') 1 else 2, p.buf.toString), from, text) )
          case _ => None
        }
      case _ => None
    }
  }

}

class SHeadingBlock( level: Int, heading: String ) extends SimpleLeafBlock {

  val name = "sheading"

  def accept(from: Int, text: String, s: Stream[String]): Option[(Int, String)] =
    if (SHeadingBlockType.sHeadingRegex.pattern.matcher( text ).matches)
      Some( (from, text) )
    else
      None

  override def toString: String = super.toString + s"[$level, $heading]"

}