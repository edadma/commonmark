package io.github.edadma.commonmark

import scala.collection.mutable

class DocumentBlock extends ContainerBlock {

  val name = "document"

  def accept(from: Int, text: String, stream: LazyList[String]): Option[(Int, String)] =
    if (blocks.nonEmpty)
      Some((from, text))
    else
      None

}
