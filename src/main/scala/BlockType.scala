package xyz.hyperreal.commonmark


abstract class BlockType {

  def start(from: Int, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser): Option[Block]

}
