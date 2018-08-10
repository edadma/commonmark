package xyz.hyperreal.commonmark


object ListBlockType extends BlockType {

  val bulletListRegex = """([-+*][ ]{1,4})(.*)"""r
  val orderedListRegex = """([0-9]{1,9}[.)])(.*)"""r
  val listRegex = """([ ]*)(.*)"""r

  def accept( indent: Int, from: Int, text: String ) =
    text match {
      case listRegex( marker, newtext ) if marker.length >= indent => Some( (from + marker.length, newtext) )
      case _ => None
    }

  def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock):
  Option[(Block, Int, String)] =
    text match {
      case bulletListRegex( marker, newtext ) =>
        Some( (new ListBlock(BulletList(marker.head, marker.length)), from + marker.length, newtext) )
      case orderedListRegex( marker, newtext ) =>
        Some( (new ListBlock(OrderedList(marker dropRight 1 toInt, marker.length)), from + marker.length, newtext) )
      case _ => None
    }

}

abstract class ListType { val indent: Int }
case class BulletList( marker: Char, indent: Int ) extends ListType
case class OrderedList( start: Int, indent: Int ) extends ListType

class ListBlock( typ: ListType ) extends ContainerBlock {

  val name = "list"
  var loose = false

  def accept( from: Int, text: String, stream: Stream[String]) : Option[(Int, String)] =
    ListBlockType.accept( typ.indent, from, text )

}