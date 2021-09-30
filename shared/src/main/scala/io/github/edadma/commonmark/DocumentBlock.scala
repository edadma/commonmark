package io.github.edadma.commonmark

import scala.collection.mutable

class DocumentBlock extends ContainerBlock {

  case class Link(url: String, title: Option[String])

  val name = "document"
  val refs = new mutable.HashMap[String, Link]

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)] =
    if (blocks nonEmpty)
      Some((from, text))
    else
      None

  override def toString: String = super.toString + s",$refs"

}
