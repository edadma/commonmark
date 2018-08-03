package xyz.hyperreal.commonmark

import scala.collection.mutable.ArrayBuffer


class CommonMarkParser {

  val blockTypes = new ArrayBuffer[BlockType]

  def parser( src: io.Source ) = {

    val doc = new DocumentBlock

    def opens = {
      def opens( b: Block ): Stream[Block] = Stream.cons( b, b.open.map(opens).getOrElse(Stream.empty) )

      opens( doc )
    }

    def next( s: Stream[String] ): Unit = {
      def matching( from: Int, b: Block ) = {
        b.accept( from, s ) match {
          case None => b.open
          case Some( newfrom ) =>
            b.open match {
              case None =>
            }
        }
      }

      if (s nonEmpty) {
        matching( 0, doc )
      }
    }

    next( src.getLines.toStream )
  }

}