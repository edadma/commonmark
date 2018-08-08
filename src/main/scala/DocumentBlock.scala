package xyz.hyperreal.commonmark


class DocumentBlock extends ContainerBlock {

  val name = "document"

  def accept( from: Int, text: String, stream: Stream[String]) : Option[(Int, String)] =
    if (blocks nonEmpty)
      Some( (from, text) )
    else
      None

}