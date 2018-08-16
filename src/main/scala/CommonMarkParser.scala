//@
package xyz.hyperreal.commonmark

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.matching.Regex


object CommonMarkParser {

  val entityReferenceRegex = "&([a-zA-Z][a-zA-Z0-9]*);"r

}

class CommonMarkParser {

  val blockTypes =
    new ArrayBuffer[BlockType] {
      append( HTMLBlockType )
      append( ReferenceBlockType )
      append( AHeadingBlockType )
      append( SHeadingBlockType )
      append( BreakBlockType )
      append( ListItemBlockType )
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

  def entities( s: String ) = {
    CommonMarkParser.entityReferenceRegex.replaceSomeIn( s, m => Entities(m group 1) )
  }

  case class C( c: Char, escaped: Boolean = false ) { override def toString = c.toString }

  def backslash( l: List[Char] ): List[C] =
    l match {
      case Nil => Nil
      case '\\' :: p :: t if "!\"#$%&'()*+,-./:;<=>?@[]^_`{|}~\\" contains p => C( p, true ) :: backslash( t )
      case c :: t => C( c ) :: backslash( t )
    }

  def entity( l: List[C] ): List[C] = {
    def parseName( l: List[C], buf: StringBuilder = new StringBuilder ): Option[(String, List[C])] =
      l match {
        case C( ';', false ) :: rest => Entities( buf.toString ) map ((_, rest))
        case C( c, false ) :: t if buf.isEmpty && c.isLetter || buf.nonEmpty && c.isLetterOrDigit =>
          buf += c
          parseName( t, buf )
        case _ => None
      }

    def parseHex( l: List[C], buf: StringBuilder = new StringBuilder ): Option[(Int, List[C])] =
      l match {
        case C( ';', false ) :: rest => Some( (Integer.parseInt(buf.toString, 16), rest) )
        case C( c, false ) :: t if "0123456789abcdefABCDEF" contains c =>
          buf += c
          parseHex( t, buf )
        case _ => None
      }

    def parseDecimal( l: List[C], buf: StringBuilder = new StringBuilder ): Option[(Int, List[C])] =
      l match {
        case C( ';', false ) :: rest => Some( (Integer.parseInt(buf.toString, 16), rest) )
        case C( c, false ) :: t if "0123456789abcdefABCDEF" contains c =>
          buf += c
          parseDecimal( t, buf )
        case _ => None
      }

    l match {
      case (c1@C( '&', false )) :: (c2@C( 'x'|'X', false )) :: t =>
        parseHex( t ) match {
          case None => c1 :: c2 :: entity( t )
          case Some( (n, rest) ) =>
            if (n < 0x10000)
              C( n.toChar, false ) :: rest
            else
              C( n.toChar, false ) :: C( n.toChar, false ) :: rest
        }
      case (c@C( '&', false )) :: t =>
        parseName( t ) match {
          case None => c :: entity( t )
          case Some( (ent, rest) ) =>
            if (ent.length == 1)
              C( ent.head, false ) :: rest
            else
              C( ent.head, false ) :: C( ent(1), false ) :: rest
        }
      case c :: t => c :: entity( t )
      case Nil => Nil
    }
  }

  def escapes( s: String ) = entity( backslash(s.toList) ) mkString

  def inline( s: String ) = TextAST( escapes(s) )

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

  def blankAfter( s: Seq[Block] ) =
    if (s.length < 2)
      false
    else
      s.sliding( 2 ).exists( a => a.head != BlankBlock && a.tail.head == BlankBlock)

  def transform( s: Stream[Block], loose: Boolean = true ): List[CommonMarkAST] =
    s match {
      case n if n isEmpty => Nil
      case h #:: t =>
          h match {
            case b: Block if !b.keep => transform( t, loose )
            case h: HTMLBlock => HTMLAST( h.buf.toString ) :: transform( t, loose )
            case _: BreakBlock => RuleAST :: transform( t, loose )
            case h: AHeadingBlock => HeadingAST( h.level, inline(h.heading), None ) :: transform( t, loose )
            case h: SHeadingBlock => HeadingAST( h.level, inlineWithHardBreaks(h.heading), None ) :: transform( t, loose )
            case p: ParagraphBlock if loose =>
              ParagraphAST( inlineWithHardBreaks(p.buf.toString) ) :: transform( t, loose )
            case p: ParagraphBlock => inlineWithHardBreaks(p.buf.toString) :: transform( t, loose )
            case b: IndentedBlock =>
              CodeBlockAST( b.buf.toString.lines.toList.reverse.dropWhile(isBlank).reverse mkString "\n", None, None ) :: transform( t, loose )
            case f: FencedBlock =>
              CodeBlockAST( f.buf.toString, if (f.info nonEmpty) Some(escapes(f.info)) else None, None ) :: transform( t, loose )
            case q: QuoteBlock => BlockquoteAST( SeqAST(transform(q.blocks.toStream)) ) :: transform( t, loose )
            case l: ListItemBlock =>
              val (items, rest) = t span (b => b.isInstanceOf[ListItemBlock] && b.asInstanceOf[ListItemBlock].typ == l.typ)
              val list = l +: items.asInstanceOf[Stream[ListItemBlock]]
              val loose1 = list.init.exists (i => blankAfter(i.blocks)) || blankAfter(list.last.blocks.init)
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