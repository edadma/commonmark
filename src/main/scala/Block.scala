package xyz.hyperreal.commonmark

import scala.collection.mutable.ArrayBuffer


abstract class Block {

  val name: String

  def accept( from: Int, stream: Stream[String] ): Option[Int]

  def add( block: Block ): Unit

  def append( from: Int, stream: Stream[String] ): Unit

  def open: Option[Block]

  override def toString: String = s"<$name>"

}

abstract class LeafBlock extends Block {

  def add( block: Block ): Unit = sys.error( "cannot add to" )

  val open = None

}

trait Appendable

abstract class TextLeafBlock extends LeafBlock with Appendable {

  val text = new StringBuilder

  def append( from: Int, stream: Stream[String] ): Unit = {
    if (text nonEmpty)
      text += '\n'

    text ++= stream.head substring from
  }

  override def toString: String = super.toString + s"""["$text"]"""

}

abstract class SpecialLeafBlock extends LeafBlock with Appendable {

  def append( from: Int, stream: Stream[String] ) = {}

}

trait NonAppendable {

  def append( from: Int, s: Stream[String] ): Unit = sys.error( "cannot append to" )

}

abstract class SimpleLeafBlock extends LeafBlock with NonAppendable

abstract class ContainerBlock extends Block with NonAppendable {

  val blocks = new ArrayBuffer[Block]

  def add( block: Block ): Unit = blocks += block

  def open = blocks lastOption

  override def toString: String = super.toString + s"[${blocks mkString ", "}]"

}
