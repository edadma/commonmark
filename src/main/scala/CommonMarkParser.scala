//@
package xyz.hyperreal.commonmark

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.matching.Regex


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

  abstract class Chr extends LeafAST
  case class Ce( text: String ) extends Chr
  case class C( text: String ) extends Chr

  def chars( l: List[Char], buf: ListBuffer[CommonMarkAST] = new ListBuffer ): List[CommonMarkAST] =
    l match {
      case Nil => buf.toList
      case '\\' :: p :: t if "!\"#$%&'()*+,-./:;<=>?@[]^_`{|}~\\" contains p =>
        buf += Ce( p.toString )
        chars( t, buf )
      case '`' :: t =>
        val (backticks, rest) = t.span( _ == '`' )

        def span( l: List[Char], buf: StringBuilder = new StringBuilder ): Option[(String, List[Char])] =
          l match {
            case Nil => None
            case '`' :: t =>
              val (b, r) = t.span( _ == '`' )

              if (b.length == backticks.length)
                Some( (buf.toString, r) )
              else {
                buf ++= "`"*(b.length + 1)
                span( r, buf )
              }
            case c :: t =>
              buf += c
              span( t, buf )
          }

        span( rest ) match {
          case None =>
            buf ++= List.fill( backticks.length + 1 )( C( "`" ) )
            chars( rest, buf )
          case Some( (code, r) ) =>
            buf += CodeSpanAST( code.trim.replaceAll("""\s+""", " ") )
            chars( r, buf )
        }
      case c :: t =>
        buf += C( c.toString )
        chars( t, buf )
    }

  def entities( l: List[CommonMarkAST] ): List[CommonMarkAST] = {
    def parseName( l: List[CommonMarkAST], buf: StringBuilder = new StringBuilder ): Option[(String, List[CommonMarkAST])] =
      l match {
        case C( ";" ) :: rest => Entities( buf.toString ) map ((_, rest))
        case C( c ) :: t if buf.isEmpty && c.head.isLetter || buf.nonEmpty && c.head.isLetterOrDigit =>
          buf ++= c
          parseName( t, buf )
        case _ => None
      }

    def parseNumeric( l: List[CommonMarkAST], digits: Char => Boolean, base: Int, buf: StringBuilder = new StringBuilder ): Option[(String, List[CommonMarkAST])] =
      l match {
        case C( ";" ) :: rest =>
          val ch =
            try {
              Integer.parseInt(buf.toString, base)
            } catch {
              case _: Exception => return None
            }

          Some( ((if (ch > 0xFFFF || ch <= 0) 0xFFFD else ch).toChar.toString, rest) )
        case C( c ) :: t if digits( c.head ) =>
          buf ++= c
          parseNumeric( t, digits, base, buf )
        case _ => None
      }

    def parseHex( l: List[CommonMarkAST], buf: StringBuilder = new StringBuilder ): Option[(String, List[CommonMarkAST])] =
      parseNumeric( l, "0123456789abcdefABCDEF" contains _, 16 )

    def parseDecimal( l: List[CommonMarkAST], buf: StringBuilder = new StringBuilder ): Option[(String, List[CommonMarkAST])] =
      parseNumeric( l, Character.isDigit, 10 )

    l match {
      case (c1@C( "&" )) :: (c2@C( "#" )) :: (c3@C( "x"|"X" )) :: t =>
        parseHex( t ) match {
          case None => c1 :: c2 :: c3 :: entities( t )
          case Some( (ent, rest) ) =>
            C( ent ) :: entities( rest )
        }
      case (c1@C( "&" )) :: (c2@C( "#" )) :: t =>
        parseDecimal( t ) match {
          case None => c1 :: c2 :: entities( t )
          case Some( (ent, rest) ) => C( ent ) :: entities( rest )
        }
      case (c@C( "&" )) :: t =>
        parseName( t ) match {
          case None => c :: entities( t )
          case Some( (ent, rest) ) => C( ent ) :: entities( rest )
        }
      case c :: t => c :: entities( t )
      case Nil => Nil
    }
  }

  def escapes( s: String ) = entities( chars(s.toList) )

  def escapedString( s: String ) = chars2string( escapes(s) )

  def chars2string( cs: List[CommonMarkAST] ) = cs map (_.asInstanceOf[Chr].text) mkString

  def breaks( l: List[CommonMarkAST], buf: ListBuffer[CommonMarkAST] = new ListBuffer ): List[CommonMarkAST] =
    l match {
      case Nil => buf.toList
      case C( " " ) :: C( " " ) :: C( "\n" ) :: t =>
        buf += HardBreakAST
        breaks( t, buf )
      case C( "\\" ) :: C( "\n" ) :: t =>
        buf += HardBreakAST
        breaks( t, buf )
      case C( "\n" ) :: t =>
        buf += SoftBreakAST
        breaks( t, buf )
      case e :: t =>
        buf += e
        breaks( t, buf )
    }

  def textual( l: List[CommonMarkAST], buf: ListBuffer[CommonMarkAST] = new ListBuffer ): List[CommonMarkAST] =
    l match {
      case Nil => buf.toList
      case (c: Chr) :: _ =>
        val (cs, r) = l span (_.isInstanceOf[Chr])

        buf += TextAST( chars2string(cs) )
        textual( r, buf )
      case e :: t =>
        buf += e
        textual( t, buf )
    }

  def inline( s: String ) = {
    val s1 = {
      if (s isEmpty)
        s
      else {
        val lines = s.lines.toSeq
        val init =
          for (l <- lines.init)
            yield {
              if (l endsWith "  ")
                l.trim + "  "
              else
                l.trim
            }

        if (init nonEmpty)
          init.mkString( "\n" ) + "\n" + lines.last.trim
        else
          lines.head.trim
      }
    }

    textual( breaks(escapes(s1)) ) match {
      case List( e ) => e
      case l => SeqAST( l )
      }
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
            case h: SHeadingBlock => HeadingAST( h.level, inline(h.heading), None ) :: transform( t, loose )
            case p: ParagraphBlock if loose =>
              ParagraphAST( inline(p.buf.toString) ) :: transform( t, loose )
            case p: ParagraphBlock => inline(p.buf.toString) :: transform( t, loose )
            case b: IndentedBlock =>
              CodeBlockAST( b.buf.toString.lines.toList.reverse.dropWhile(isBlank).reverse mkString "\n", None, None ) :: transform( t, loose )
            case f: FencedBlock =>
              CodeBlockAST( f.buf.toString, if (f.info nonEmpty) Some(escapedString(f.info)) else None, None ) ::
                transform( t, loose )
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