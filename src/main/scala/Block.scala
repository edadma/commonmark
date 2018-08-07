package xyz.hyperreal.commonmark

import scala.collection.mutable.ArrayBuffer


abstract class Block {

  val name: String
  var keep = true

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)]

  def add( block: Block ): Unit

  def append(from: Int, text: String, stream: Stream[String]): Unit

  def isAppendable: Boolean

  val isInterruptible = true

  def open: Option[Block]

  override def toString: String = s"<${if (!keep) "*" else ""}$name>"

}

abstract class LeafBlock extends Block {

  def add( block: Block ): Unit = sys.error( "cannot add to" )

  val open = None

}

trait Appendable {

  val isAppendable = true

}

abstract class TextLeafBlock extends LeafBlock with Appendable {

  val buf = new StringBuilder

  def append(from: Int, text: String, stream: Stream[String]): Unit = {
    if (buf nonEmpty)
      buf += '\n'

    buf ++= text
  }

  override def toString: String = super.toString + s"""["$buf"]"""

}

trait NonAppendable {

  def append( from: Int, text: String, s: Stream[String] ): Unit = sys.error( "cannot append to" )

  val isAppendable = false

}

abstract class SimpleLeafBlock extends LeafBlock with NonAppendable

abstract class ContainerBlock extends Block with NonAppendable {

  val blocks = new ArrayBuffer[Block]

  def add( block: Block ): Unit = blocks += block

  def open = blocks lastOption

  override def toString: String = super.toString + s"[${blocks mkString ", "}]"

}
