//@
package xyz.hyperreal.commonmark

import scala.collection.mutable.{ArrayBuffer, ListBuffer}


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
                b.start( f, t, s, p, this, doc ) match {
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

  def inline( s: String ) = TextAST( s )

  def inlineWithHardBreaks( s: String ) = {
    val seq = new ListBuffer[CommonMarkAST]
    val lines = s.lines.toArray

    def add( ast: CommonMarkAST* ) =
      ast foreach {
        case SeqAST( s ) => seq ++= s
        case a => seq += a
      }

    for (l <- lines.init)
      if (l endsWith "  ")
        add( inline(l.trim), HardBreakAST )
      else if (l endsWith "\\")
        add( inline(l.trim dropRight 1), HardBreakAST )
      else
        add( inline(l.trim), SoftBreakAST )

    add( inline(lines.last.trim) )

    if (seq.length == 1)
      seq.head
    else
      SeqAST( seq.toList )
  }

  def transform( s: Stream[Block], loose: Boolean = true ): List[CommonMarkAST] =
    s match {
      case n if n isEmpty => Nil
      case h #:: t =>
          h match {
            case b: Block if !b.keep => transform( t, loose )
            case _: BreakBlock => RuleAST :: transform( t, loose )
            case h: AHeadingBlock => HeadingAST( h.level, inline(h.heading), None ) :: transform( t, loose )
            case h: SHeadingBlock => HeadingAST( h.level, inlineWithHardBreaks(h.heading), None ) :: transform( t, loose )
            case p: ParagraphBlock if loose => ParagraphAST( inlineWithHardBreaks(p.buf.toString) ) :: transform( t, loose )
            case p: ParagraphBlock => inlineWithHardBreaks(p.buf.toString) :: transform( t, loose )
            case b: IndentedBlock =>
              CodeBlockAST( b.buf.toString.lines.toList.reverse.dropWhile(isBlank).reverse mkString "\n", None, None ) :: transform( t, loose )
            case f: FencedBlock => CodeBlockAST( f.buf.toString, if (f.info nonEmpty) Some(f.info) else None, None ) :: transform( t, loose )
            case q: QuoteBlock => BlockquoteAST( SeqAST(transform(q.blocks.toStream)) ) :: transform( t, loose )
            case l: ListBlock =>
              val (items, rest) = t span (b => b.isInstanceOf[ListBlock] && b.asInstanceOf[ListBlock].typ == l.typ)
              val list = l +: items.asInstanceOf[Stream[ListBlock]]
              val loose1 = list exists (_.blocks.exists(_ == BlankBlock))
              val listitems = list map (b => ListItemAST( SeqAST(transform(b.blocks.toStream, loose1)) )) toList
              val hd =
                if (l.typ.isInstanceOf[BulletList])
                  BulletListAST( SeqAST(listitems), !loose1 )
                else
                  OrderedListAST( SeqAST(listitems), !loose1, l.typ.asInstanceOf[OrderedList].start )

              hd :: transform( rest, loose )
          }

    }

}