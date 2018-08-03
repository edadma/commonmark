package xyz.hyperreal.commonmark

import scala.collection.mutable.ArrayBuffer


class CommonMarkParser {

  trait BlockType
  case object Break extends BlockType
  case object AHeading extends BlockType
  case object SHeading extends BlockType
  case object ICode extends BlockType
  case object FCode extends BlockType
  case object HTML extends BlockType
  case object Ref extends BlockType
  case object Par extends BlockType
  case object Blank extends BlockType

  case object Doc extends BlockType
  case object Quote extends BlockType
  case object Lis extends BlockType
  case class Bullet( c: Char ) extends BlockType
  case class Ordered( c: Char ) extends BlockType

  trait Block
  case class LeafBlock( var typ: BlockType, content: StringBuilder ) extends Block
  case class ContainerBlock( var typ: BlockType, content: ArrayBuffer[Block] ) extends Block

  def parser( src: io.Source ) = {

    val doc = ContainerBlock( Doc, new ArrayBuffer[Block] )

    for (line <- src.getLines) {

    }
  }

}