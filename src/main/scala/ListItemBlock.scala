//@
package xyz.hyperreal.commonmark


object ListItemBlockType extends BlockType {

  val bulletListRegex = """([ ]{0,3})([-+*])(?:([ ]+)([^ ].*)|\s*)"""r
  val orderedListRegex = """([ ]{0,3})([0-9]{1,9})([.)])(?:([ ]+)([^ ].*)|\s*)"""r
  val listRegex = """([ ]*)(.*)"""r

  def accept( list: ListItemBlock, from: Int, text: String ) =
    if (isBlank( text ) && list.blocks.exists( _ != BlankBlock ))
      Some( (from, text) )
    else
      text match {
        case listRegex( spaces, newtext ) =>
          if (spaces.length >= list.indent)
            Some( (from + list.indent, " "*(spaces.length - list.indent) + newtext) )
          else
            None
        case _ => None
      }

  def start(from: Int, text: String, s: LazyList[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock):
  Option[(Block, Int, String)] =
    text match {
      case bulletListRegex( indent, marker, spaces, newtext ) =>
        if (spaces eq null) {
          prev.open match {
            case Some( _: ParagraphBlock ) => None
            case _ =>
              val width = 2 + indent.length

              Some( (new ListItemBlock(width, BulletList(marker.head)), from + width, "") )
          }
        } else {
          val sep = if (spaces.length > 4) 1 else spaces.length
          val width = 1 + indent.length + sep

          Some( (new ListItemBlock(width, BulletList(marker.head)), from + width, " "*(spaces.length - sep) + newtext) )
        }
      case orderedListRegex( indent, number, marker, spaces, newtext ) =>
        if (spaces eq null) {
          prev.open match {
            case Some( _: ParagraphBlock ) => None
            case _ =>
              val width = 2 + number.length + indent.length

              Some( (new ListItemBlock(width, OrderedList(marker.head)) { typ.asInstanceOf[OrderedList].start = number.toInt }, from + width, "") )
          }
        } else {
          prev.open match {
            case Some( _: ParagraphBlock ) if number != "1" => None
            case _ =>
            val sep = if (spaces.length > 4) 1 else spaces.length
            val width = 1 + number.length + indent.length + sep

            Some( (new ListItemBlock(width, OrderedList(marker.head)) { typ.asInstanceOf[OrderedList].start = number.toInt },
              from + width, " "*(spaces.length - sep) + newtext) )
          }
        }
      case _ => None
    }

}

abstract class ListType
case class BulletList( marker: Char ) extends ListType
case class OrderedList( marker: Char ) extends ListType { var start = 1 }

class ListItemBlock( val indent: Int, val typ: ListType ) extends ContainerBlock {

  val name = "list"
  var tight = true

  def accept( from: Int, text: String, stream: LazyList[String]) : Option[(Int, String)] =
    ListItemBlockType.accept( this, from, text )

}