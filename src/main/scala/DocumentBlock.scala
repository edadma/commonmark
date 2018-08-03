package xyz.hyperreal.commonmark


class DocumentBlock extends ContainerBlock {

  def accept( from: Int, stream: Stream[String] ) = Some( from )

}