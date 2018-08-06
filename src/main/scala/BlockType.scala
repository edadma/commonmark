package xyz.hyperreal.commonmark


abstract class BlockType {

  def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser): Option[(Block, Int, String)]

}
