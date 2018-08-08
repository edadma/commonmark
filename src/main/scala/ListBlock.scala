package xyz.hyperreal.commonmark


object ListBlockType extends BlockType {

  def start( from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser ):
  Option[(Block, Int, String)] =
    if (text startsWith "> ")
      Some( (new ListBlock, from + 2, text substring 2) )
    else if (text == ">")
      Some( (new ListBlock, from + 1, "") )
    else
      None

}

class ListBlock extends ContainerBlock {

  val name = "list"

  def accept( from: Int, text: String, stream: Stream[String]) : Option[(Int, String)] =
    if (text startsWith "> ")
      Some( (from + 2, text substring 2) )
    else if (text == ">")
      Some( (from + 1, "") )
    else
      None

}