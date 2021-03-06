package xyz.hyperreal.commonmark


object ReferenceBlockType extends BlockType {

  val linkRegex = """[ ]{0,3}\[([^\]]+)\]:\s*(.+?)\s*(?:"(.*?)")?"""r

  override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock): Option[(Block, Int, String)] =
    text match {
      case linkRegex( ref, url, title ) =>
        doc.refs(ref) = doc.Link( url, if (title eq null) None else Some(title) )
        Some( (ReferenceBlock, from, text) )
      case _ => None
    }

}

object ReferenceBlock extends SimpleLeafBlock {  // this is an object and not a class for efficiency

  val name = "reference"

  keep = false

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)] = Some( (from, text) )

}