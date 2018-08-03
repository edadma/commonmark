package xyz.hyperreal.commonmark

import scala.collection.mutable.ArrayBuffer


abstract class Block {

  def accept( from: Int, stream: Stream[String] ): Option[Int]

  def add( block: Block ): Unit

  def append( from: Int, stream: Stream[String] ): Unit

  def open: Option[Block]

}

abstract class LeafBlock extends Block {

  def accept( from: Int, stream: Stream[String] ): Option[Int]

  def add( block: Block ): Unit = sys.error( "cannot add to" )

  def append( from: Int, stream: Stream[String] ): Unit

  val open = None

}

abstract class TextLeafBlock extends LeafBlock {

  val text = new StringBuilder

  def accept( from: Int, stream: Stream[String] ): Option[Int]

  override def append( from: Int, stream: Stream[String] ): Unit = {
    text += '\n'
    text ++= stream.head substring from
  }

}

trait NonAppendable {

  def append( from: Int, s: Stream[String] ): Unit = sys.error( "cannot append to" )

}

abstract class SimpleLeafBlock extends LeafBlock with NonAppendable {

  def accept( from: Int, stream: Stream[String] ): Option[Int]

}

abstract class ContainerBlock extends Block with NonAppendable {

  val blocks = new ArrayBuffer[Block]

  def accept( from: Int, stream: Stream[String] ): Option[Int]

  def add( block: Block ): Unit = blocks += block

  def open = blocks lastOption

}
