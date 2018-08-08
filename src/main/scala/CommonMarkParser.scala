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
      append( FencedBlockType )
      append( QuoteBlockType )
      append( ParagraphBlockType )
      append( BlankBlockType )
    }

  def parse( src: String ): DocumentBlock = parse( io.Source.fromString(src) )

  def parse( src: io.Source ) = {

    val doc = new DocumentBlock
    val trail = new ArrayBuffer[Block]

    trail += doc

    def next( s: Stream[String] ): Unit = {
      def matching( from: Int, text: String, prev: ContainerBlock, blocks: List[Block] ): (Block, Int, String, ContainerBlock) =
        blocks match {
          case Nil => (null, from, text, prev)
          case b :: t =>
            b.accept(from, text, s) match {
              case None => (b, from, text, prev)
              case Some( (newfrom, newtext) ) =>
                matching( newfrom, newtext, if (b.isInstanceOf[ContainerBlock]) b.asInstanceOf[ContainerBlock] else prev, t )
            }
        }
//      println( st )

      if (s nonEmpty) {
        matching( 0, s.head, doc, trail.toList ) match {
          case (block, from, text, prev) =>
            def start( f: Int, t: String, p: ContainerBlock ): Option[(Block, Int, String)] = {
              for (b <- blockTypes)
                b.start(f, t, s, p, this) match {
                  case None =>
                  case st => return st
                }

              None
            }

            val (newfrom, newtext) =
              prev.open match {
                case Some( b ) if b != block && !b.isInterruptible => (from, text)
                case _ =>
                  def starts( from: Int, text: String ): (Int, String) = {
                    start( from, text, prev ) match {
                      case None => (from, text)
                      case Some( (st, fr, tx) ) =>
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

                        if (st.isInstanceOf[ContainerBlock])
                          starts( fr, tx )
                        else
                          (fr, tx)
                    }
                  }

                  starts( from, text )
              }

              if (trail.last.isAppendable)
                trail.last.append(newfrom, newtext, s)
        }

        next( s.tail )
      }
    }

    next( src.getLines.toStream )
    doc
  }

}