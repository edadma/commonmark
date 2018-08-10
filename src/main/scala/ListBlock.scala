package xyz.hyperreal.commonmark

import scala.collection.mutable.ListBuffer


object ListBlockType extends BlockType {

  val bulletListRegex = """([-+*][ ]{1,4})(.*)"""r
  val orderedListRegex = """([0-9]{1,9}[.)])(.*)"""r
  val listRegex = """([ ]*)(.*)"""r

  def accept( list: ListBlock, from: Int, text: String ) =
    if (isBlank( text ))
      Some( (from, text) )
    else
      text match {
        case listRegex( marker, newtext ) if marker.length >= list.typ.indent => Some( (from + marker.length, newtext) )
//        case bulletListRegex( marker, newtext ) =>
//          list.typ match {
//            case BulletList( m, _ ) if marker.head == m =>
//              list.items += list.blocks.toList
//              list.blocks.clear
//              Some( (from + marker.length, newtext) )
//            case _ => None
//          }
//        case orderedListRegex( marker, newtext ) =>
//          Some( (new ListBlock(OrderedList(marker dropRight 1 toInt, marker.length)), from + marker.length, newtext) )
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

class ListBlock( val typ: ListType ) extends ContainerBlock {

  val name = "list"
  val items = new ListBuffer[List[Block]]
  var loose = false

  def accept( from: Int, text: String, stream: Stream[String]) : Option[(Int, String)] =
    ListBlockType.accept( this, from, text )

}