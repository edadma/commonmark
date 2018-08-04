package xyz.hyperreal.commonmark

import scala.collection.mutable.ArrayBuffer


class CommonMarkParser {

  val blockTypes =
    new ArrayBuffer[BlockType] {
      append( ParagraphBlockType )
      append( BlankBlockType )
    }

  def parser( src: io.Source ) = {

    val doc = new DocumentBlock
    val trail = new ArrayBuffer[Block]

    trail += doc

//    def opens = {
//      def opens( b: Block ): Stream[Block] = Stream.cons( b, b.open.map(opens).getOrElse(Stream.empty) )
//
//      opens( doc )
//    }

    def next( s: Stream[String] ): Unit = {
      def matching( from: Int, prev: Block, blocks: List[Block] ): (Boolean, Int, Block) =
        blocks match {
          case Nil => (true, from, prev)
          case b :: t =>
            b.accept( from, s ) match {
              case None => (false, from, prev)
              case Some( newfrom ) => matching( newfrom, if (b.isInstanceOf[ContainerBlock]) b else prev, t )
            }
        }

      if (s nonEmpty) {
        matching( 0, doc, trail.toList ) match {
          case (false, from, prev) => trail.last.append( from, s )
          case (true, from, prev) =>
        }
      }
    }

    next( src.getLines.toStream )
  }

}