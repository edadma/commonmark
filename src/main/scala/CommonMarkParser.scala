//@
package xyz.hyperreal.commonmark

import scala.collection.mutable.ArrayBuffer


class CommonMarkParser {

  val blockTypes =
    new ArrayBuffer[BlockType] {
      append( ReferenceBlockType )
      append( AHeadingBlockType )
      append( SHeadingBlockType )
      append( BreakBlockType )
      append( ListBlockType )
      append( IndentedBlockType )
      append( FencedBlockType )
      append( QuoteBlockType )
      append( ParagraphBlockType )
      append( BlankBlockType )
    }

  def parse( src: String ): CommonMarkAST = parse( io.Source.fromString(src) )

  def parse( src: io.Source ) = SeqAST( transform(parseBlocks(src.getLines.toStream).blocks.toStream) )

  def parseBlocks( lines: Stream[String] ) = {
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
                b.start(f, t, s, p, this, doc ) match {
                  case None =>
                  case st => return st
                }

              None
            }

            val (newfrom, newtext) =
              prev.open match {
                case Some( b ) if b != block && !b.isInterruptible => (from, text)
                case _ =>
                  def starts( from: Int, text: String, prev: ContainerBlock ): (Int, String) = {
                    start( from, text, prev ) match {
                      case None => (from, text)
                      case Some( (st, fr, tx) ) =>
                        def add: Unit = {
                          prev.add( st )
                          trail += st
                        }

                        if (!(st.isInstanceOf[ParagraphBlock] && trail.last.isInstanceOf[ParagraphBlock]))
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
                          starts( fr, tx, st.asInstanceOf[ContainerBlock] )
                        else
                          (fr, tx)
                    }
                  }

                  starts( from, text, prev )
              }

              if (trail.last.isAppendable)
                trail.last.append(newfrom, newtext, s)
        }

        next( s.tail )
      }
    }

    next( lines )
    doc
  }

  def transform( s: Stream[Block], loose: Boolean = true ): List[CommonMarkAST] =
    s match {
      case n if n isEmpty => Nil
      case h #:: t =>
          h match {
            case b: Block if !b.keep => transform( t )
            case _: BreakBlock => RuleAST :: transform( t )
            case h: SHeadingBlock => HeadingAST( h.level, TextAST(h.heading), None ) :: transform( t )
            case p: ParagraphBlock if loose => ParagraphAST( TextAST(p.buf.toString) ) :: transform( t, loose )
            case p: ParagraphBlock => TextAST(p.buf.toString) :: transform( t )
            case b: IndentedBlock => CodeBlockAST( b.buf.toString, None, None ) :: transform( t, loose )
            case q: QuoteBlock => BlockquoteAST( SeqAST(transform(q.blocks.toStream)) ) :: transform( t, loose )
            case l: ListBlock =>
              val (items, rest) = t span (b => b.isInstanceOf[ListBlock] && b.asInstanceOf[ListBlock].typ == l.typ)
              val list = l +: items.asInstanceOf[Stream[ListBlock]]
              val loose1 = list dropRight 1 exists (_.blocks.last == BlankBlock)
              val listitems = list map (b => ListItemAST( SeqAST(transform(b.blocks.toStream, loose1)) )) toList
              val hd =
                if (l.typ.isInstanceOf[BulletList])
                  BulletListAST( SeqAST(listitems), !loose1 )
                else
                  OrderedListAST( SeqAST(listitems), !loose1 )

              hd :: transform( rest, loose )
          }

    }

}