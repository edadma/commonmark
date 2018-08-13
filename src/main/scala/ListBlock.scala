//@
package xyz.hyperreal.commonmark


object ListBlockType extends BlockType {

  val bulletListRegex = """([ ]{0,3})([-+*])(?:([ ]+)(.*)|\s*)"""r
  val orderedListRegex = """([ ]{0,3})([0-9]{1,9})([.)])(?:([ ]+)(.*)|\s*)"""r
  val listRegex = """([ ]*)(.*)"""r

  def accept( list: ListBlock, from: Int, text: String ) =
    if (isBlank( text ))
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

  def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock):
  Option[(Block, Int, String)] =
    text match {
      case bulletListRegex( indent, marker, spaces, newtext ) =>
        if (spaces eq null) {
          prev.open match {
            case Some( _: ParagraphBlock ) => None
            case _ =>
              val width = 1 + indent.length

              Some( (new ListBlock(width, BulletList(marker.head)), from + width, "") )
          }
        } else {
          val width = 1 + indent.length + spaces.length

          Some( (new ListBlock(width, BulletList(marker.head)), from + width, newtext) )
        }
      case orderedListRegex( indent, number, marker, spaces, newtext ) =>
        if (spaces eq null) {
          prev.open match {
            case Some( _: ParagraphBlock ) => None
            case _ =>
              val width = 1 + number.length + indent.length

              Some( (new ListBlock(width, OrderedList(marker.head)) { typ.asInstanceOf[OrderedList].start = number.toInt }, from + width, "") )
          }
        } else {
          val width = 1 + number.length + indent.length + spaces.length

          Some( (new ListBlock(width, OrderedList(marker.head)) { typ.asInstanceOf[OrderedList].start = number.toInt }, from + width, newtext) )
        }
      case _ => None
    }

}

abstract class ListType
case class BulletList( marker: Char ) extends ListType
case class OrderedList( marker: Char ) extends ListType { var start = 1 }

class ListBlock( val indent: Int, val typ: ListType ) extends ContainerBlock {

  val name = "list"
  var tight = true

  def accept( from: Int, text: String, stream: Stream[String]) : Option[(Int, String)] =
    ListBlockType.accept( this, from, text )

}