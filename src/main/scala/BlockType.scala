package xyz.hyperreal.commonmark


abstract class BlockType {

  def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock): Option[(Block, Int, String)]

}
