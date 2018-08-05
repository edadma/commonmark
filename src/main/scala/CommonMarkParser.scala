//@
package xyz.hyperreal.commonmark

import scala.collection.mutable.ArrayBuffer


class CommonMarkParser {

  val blockTypes =
    new ArrayBuffer[BlockType] {
      append( AHeadingBlockType )
      append( BreakBlockType )
      append( ParagraphBlockType )
      append( BlankBlockType )
    }

  def parse( src: String ): DocumentBlock = parse( io.Source.fromString(src) )

  def parse( src: io.Source ) = {

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
          case (_, from, prev) =>
            def start: Option[Block] = {
              for (b <- blockTypes)
                b.start( from, s ) match {
                  case None =>
                  case s => return s
                }

              None
            }

            start match {
              case None =>
              case Some( st ) =>
                def add: Unit = {
                  prev.add( st )
                  trail += st
                }

                prev.open match {
                  case None => add
                  case Some( b ) =>
                    if (!(st.isAppendable && b.isAppendable && st.getClass == b.getClass)) {
                      trail.reverseIterator indexWhere (_ == b) match {
                        case -1 => sys.error( "problem" )
                        case idx => trail.remove( trail.length - 1 - idx, idx + 1 )
                      }

                      add
                    }
                }
            }

            if (trail.last.isAppendable)
              trail.last.append( from, s )
        }

        next( s.tail )
      }
    }

    next( src.getLines.toStream )
    doc
  }

}