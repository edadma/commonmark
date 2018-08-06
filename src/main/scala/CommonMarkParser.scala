//@
package xyz.hyperreal.commonmark

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class CommonMarkParser {

  private [commonmark] val refs = new mutable.HashMap[String, Link]

  case class Link( url: String, title: Option[String] )

  val blockTypes =
    new ArrayBuffer[BlockType] {
      append( AHeadingBlockType )
      append( SHeadingBlockType )
      append( BreakBlockType )
      append( IndentedBlockType )
      append( ParagraphBlockType )
      append( BlankBlockType )
    }

  def parse( src: String ): DocumentBlock = parse( io.Source.fromString(src) )

  def parse( src: io.Source ) = {

    val doc = new DocumentBlock
    val trail = new ArrayBuffer[Block]

    trail += doc

    def next( s: Stream[String] ): Unit = {
      def matching( from: Int, prev: ContainerBlock, blocks: List[Block] ): (Boolean, Int, ContainerBlock) =
        blocks match {
          case Nil => (true, from, prev)
          case b :: t =>
            b.accept( from, s ) match {
              case None => (false, from, prev)
              case Some( newfrom ) =>
                matching( newfrom, if (b.isInstanceOf[ContainerBlock]) b.asInstanceOf[ContainerBlock] else prev, t )
            }
        }

      if (s nonEmpty) {
        matching( 0, doc, trail.toList ) match {
          case (_, from, prev) =>
            def start: Option[(Block, Int)] = {
              for (b <- blockTypes)
                b.start( from, s, prev, this ) match {
                  case None =>
                  case st => return st
                }

              None
            }

            val newfrom =
              start match {
                case None => from
                case Some( (st, fr) ) =>
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

                  fr
              }

            if (trail.last.isAppendable)
              trail.last.append( newfrom, s )
        }

        next( s.tail )
      }
    }

    next( src.getLines.toStream )
    doc
  }

}