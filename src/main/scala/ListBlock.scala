package xyz.hyperreal.commonmark


object ListBlockType extends BlockType {

  val bulletListRegex = """([-+*])[ ]{1,4}(.*)"""r
  val orderedListRegex = """([0-9]{1,9})([.)])[ ]{1,4}(.*)"""r
  val listRegex = """([ ]*)(.*)"""r

  def accept( list: ListBlock, from: Int, text: String ) =
    if (isBlank( text ))
      Some( (from, text) )
    else
      text match {
        case listRegex( marker, newtext ) if marker.length >= 2 => Some( (from + marker.length, newtext) )
        case _ => None
      }

  def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock):
  Option[(Block, Int, String)] =
    text match {
      case bulletListRegex( marker, newtext ) =>
        Some( (new ListBlock(BulletList(marker.head)), from + 2, newtext) )
      case orderedListRegex( number, marker, newtext ) =>
        Some( (new ListBlock(new OrderedList(marker.head) { override val start = number.toInt }), from + 2, newtext) )
      case _ => None
    }

}

abstract class ListType
case class BulletList( marker: Char ) extends ListType
case class OrderedList( marker: Char ) extends ListType { val start = 1 }

class ListBlock( val typ: ListType ) extends ContainerBlock {

  val name = "list"
  var tight = true

  def accept( from: Int, text: String, stream: Stream[String]) : Option[(Int, String)] =
    ListBlockType.accept( this, from, text )

}