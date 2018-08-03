package xyz.hyperreal.commonmark


class ParagraphHandler extends BlockHandler {

  override def open(s: Stream[String]): Option[CommonMarkParser#Block] = new CommonMarkParser#Block( )

  override def close(s: Stream[String]): Boolean = ???

}