package xyz.hyperreal.commonmark


object ListBlockType extends BlockType {

  val listRegex = """([-+*][ ]{1,4}|[0-9]{1,9}[.)])(.*)"""r

  def accept( from: Int, text: String ) =
    text match {
      case listRegex( marker, newtext ) => Some( (from + marker.length, newtext) )
      case _ => None
    }

  def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock):
  Option[(Block, Int, String)] =
    accept( from, text ) match {
      case Some( (marker, newtext) ) => Some( (new QuoteBlock, from + marker, newtext) )
      case _ => None
    }

}

class ListBlock extends ContainerBlock {

  val name = "list"

  def accept( from: Int, text: String, stream: Stream[String]) : Option[(Int, String)] =
    QuoteBlockType.accept( from, text )

}