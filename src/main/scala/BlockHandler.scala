package xyz.hyperreal.commonmark


abstract class BlockHandler {

  def open( s: Stream[String] ): Option[CommonMarkParser#Block]

  def close( s: Stream[String] ): Boolean

}