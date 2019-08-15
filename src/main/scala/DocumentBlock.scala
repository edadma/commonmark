package xyz.hyperreal.commonmark

import scala.collection.mutable


class DocumentBlock extends ContainerBlock {

  case class Link( url: String, title: Option[String] )

  val name = "document"
  val refs = new mutable.HashMap[String, Link]

  def accept( from: Int, text: String, stream: LazyList[String] ) : Option[(Int, String)] =
    if (blocks nonEmpty)
      Some( (from, text) )
    else
      None

  override def toString: String = super.toString + s",$refs"

}