package xyz.hyperreal.commonmark


abstract class BlockType {

  def start( from: Int, s: Stream[String] ): Option[Block]

}
