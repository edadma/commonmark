package xyz.hyperreal.commonmark


class DocumentBlock extends ContainerBlock {

  val name = "document"

  def accept( from: Int, stream: Stream[String] ) = Some( from )

}