//@
package io.github.edadma.commonmark

import io.github.edadma.recognizer.Input

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.language.postfixOps
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

object CommonMarkParser {

  val emailRegex: Regex =
    """[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*""" r
  val uriRegex: Regex = """[a-zA-Z][a-zA-Z0-9+.-]{1,31}:[^\s<>]*""" r
  val htmlRegex: Regex =
    """(?isx)
      |# start tag
      |<[a-z][a-z0-9-]*(?:\s+[a-z_:][a-z90-9_.:-]*(?:\s*=\s*(?:[^\s"'=<>`]+|'[^']*'|"[^"]*"))?)*\s*/?>|
      |
      |# end tag
      |</[a-z][a-z0-9-]*\s*>|
      |
      |# comment
      |<!--[^>](?:.(?!--))*?[^-]-->|
      |
      |# instruction
      |<\?.*?\?>|
      |
      |# declaration
      |<![A-Z]\s*[^>]*>|
      |
      |# CDATA
      |<!\[CDATA\[.*?]]>
    """.stripMargin.r
}

class CommonMarkParser {

  val blockTypes: mutable.Seq[BlockType] =
    new ArrayBuffer[BlockType] {
      append(HTMLBlockType)
      append(ReferenceBlockType)
      append(AHeadingBlockType)
      append(SHeadingBlockType)
      append(BreakBlockType)
      append(ListItemBlockType)
      append(IndentedBlockType)
      append(FencedBlockType)
      append(QuoteBlockType)
      append(ParagraphBlockType)
      append(BlankBlockType)
    }
  val refs = new mutable.HashMap[String, LinkInfo]
  val linksImagesParser = new LinksImagesParser(this)

  def parse(src: String): CommonMarkAST = parse(scala.io.Source.fromString(src))

  def expandTabs(s: String): String =
    if (s contains '\t') {
      val buf = new StringBuilder

      s foreach {
        case '\t' => buf ++= "\ue000" * (4 - buf.length % 4)
        case c    => buf += c
      }

      buf.toString
    } else s

  def parse(src: scala.io.Source): SeqAST =
    SeqAST(transform(parseBlocks(src.getLines() to LazyList map expandTabs).blocks to LazyList))

  def parseBlocks(lines: LazyList[String]): DocumentBlock = {
    val doc = new DocumentBlock
    val trail = new ArrayBuffer[Block]

    trail += doc

    @tailrec
    def next(s: LazyList[String]): Unit = {
      @tailrec
      def matching(from: Int,
                   text: String,
                   prev: ContainerBlock,
                   blocks: List[Block]): (Block, Int, String, ContainerBlock) =
        blocks match {
          case Nil => (null, from, text, prev)
          case b :: t =>
            b.accept(from, text, s) match {
              case None => (b, from, text, prev)
              case Some((newfrom, newtext)) =>
                matching(newfrom,
                         newtext,
                         if (b.isInstanceOf[ContainerBlock]) b.asInstanceOf[ContainerBlock] else prev,
                         t)
            }
        }

      if (s nonEmpty) {
        matching(0, s.head, doc, trail.toList) match {
          case (block, from, text, prev) =>
            def start(f: Int, t: String, p: ContainerBlock): Option[(Block, Int, String)] = {
              for (b <- blockTypes)
                b.start(f, t, s, p, this, doc) match {
                  case None =>
                  case st   => return st
                }

              None
            }

            val (newfrom, newtext) =
              prev.open match {
                case Some(b) if b != block && !b.isInterruptible => (from, text)
                case _ =>
                  def starts(from: Int, text: String, prev: ContainerBlock): (Int, String) = {
                    start(from, text, prev) match {
                      case None => (from, text)
                      case Some((st, fr, tx)) =>
                        def add: Unit = {
                          prev.add(st)
                          trail += st
                        }

                        if (!(st.isInstanceOf[ParagraphBlock] && trail.last.isInstanceOf[ParagraphBlock]))
                          prev.open match {
                            case None => add
                            case Some(b) =>
                              if (!(st.isAppendable && b.isAppendable && st.getClass == b.getClass)) {
                                trail.reverseIterator indexWhere (_ == b) match {
                                  case -1  => sys.error("problem")
                                  case idx => trail.remove(trail.length - 1 - idx, idx + 1)
                                }

                                add
                              }
                          }

                        if (st.isInstanceOf[ContainerBlock])
                          starts(fr, tx, st.asInstanceOf[ContainerBlock])
                        else
                          (fr, tx)
                    }
                  }

                  starts(from, text, prev)
              }

            if (trail.last.isAppendable)
              trail.last.append(newfrom, newtext, s)
        }

        next(s.tail)
      }
    }

    next(lines)
    doc
  }

  abstract class Chr extends LeafAST

  case class Ce(text: String) extends Chr

  case class C(text: String) extends Chr {
    var leftFlanking = false
    var rightFlanking = false
    var followedByPunct = false
    var precededByPunct = false
  }

  class CommonMarkArrayInput(private val array: collection.Seq[CommonMarkAST], private val idx: Int)
      extends Input[CommonMarkAST, Char] {
    def eoi: Boolean = idx >= array.length

    def elem: Char =
      array(idx) match {
        case C(c) => c.head
        case _    => '\u0000'
      }

    def wrapped: CommonMarkAST = array(idx)

    def next: CommonMarkArrayInput = new CommonMarkArrayInput(array, idx + 1)

    override def equals(obj: Any): Boolean =
      obj.asInstanceOf[CommonMarkArrayInput].array.eq(array) && obj
        .asInstanceOf[CommonMarkArrayInput]
        .idx == idx
  }

  def chars(l: List[Char], buf: ListBuffer[CommonMarkAST] = new ListBuffer, col: Int = 0): List[CommonMarkAST] =
    l match {
      case Nil => buf.toList
      case '\\' :: p :: t if "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~" contains p =>
        buf += Ce(p.toString)
        chars(t, buf)
      case '<' :: t =>
        t indexOf '>' match {
          case -1 =>
            buf += C("<")
            chars(t, buf)
          case idx =>
            val (link, rest) = t splitAt idx
            val addr = link mkString

            if (CommonMarkParser.uriRegex.pattern.matcher(addr).matches) {
              buf += URIAutolinkAST(addr)
              chars(rest.tail, buf)
            } else if (CommonMarkParser.emailRegex.pattern.matcher(addr).matches) {
              buf += EmailAutolinkAST(addr)
              chars(rest.tail, buf)
            } else {
              val s = l.mkString
              val m = CommonMarkParser.htmlRegex.pattern.matcher(s)

              if (m.lookingAt) {
                buf += RawHTMLAST(m group 0)
                chars(l drop m.end, buf)
              } else {
                buf += C("<")
                chars(t, buf)
              }
            }
        }
      case '`' :: t =>
        val (backticks, rest) = t.span(_ == '`')

        @tailrec
        def span(l: List[Char], buf: StringBuilder = new StringBuilder): Option[(String, List[Char])] =
          l match {
            case Nil => None
            case '`' :: t =>
              val (b, r) = t.span(_ == '`')

              if (b.length == backticks.length)
                Some((buf.toString, r))
              else {
                buf ++= "`" * (b.length + 1)
                span(r, buf)
              }
            case c :: t =>
              buf += c
              span(t, buf)
          }

        span(rest) match {
          case None =>
            buf ++= List.fill(backticks.length + 1)(C("`"))
            chars(rest, buf)
          case Some((code, r)) =>
            val nomalized = {
              val s = code.replace("\r\n", " ").replace('\r', ' ').replace('\n', ' ')

              if (s.length >= 2 && s.head == ' ' && s.last == ' ' && !s.forall(_ == ' ')) s drop 1 dropRight 1
              else s

              //              val s = new StringBuilder(code)
              //              var i = 0
              //
              //              while (i >= 0 && (s(i) == '\n' || s(i) == '\r')) {
              //                if (s(i) == '\n' && (i == 0 || s(i - 1) != '\r'))
              //                  s(i) = ' '
              //                else if (s(i) == '\n' && i > 0 && s(i - 1) == '\r') {
              //                  s(i - 1) = ' '
              //                  s.deleteCharAt(i)
              //                  i -= 1
              //                } else if (s(i) == '\r')
              //                  s(i) = ' '
              //
              //                i -= 1
              //              }
              //
              //              if (s.length >= 2 && s.head == ' ' && s.last == ' ' && !s.forall(_ == ' ')) {
              //                s.deleteCharAt(s.length - 1)
              //                s.deleteCharAt(0)
              //              }
              //
              //              s.toString
            }

            buf += CodeSpanAST(nomalized)
            chars(r, buf)
        }
      case c :: t =>
        buf += C(c.toString)
        chars(t, buf)
    }

  def entities(l: List[CommonMarkAST], buf: ListBuffer[CommonMarkAST], appends: CommonMarkAST*): List[CommonMarkAST] = {
    def parseName(l: List[CommonMarkAST],
                  buf: StringBuilder = new StringBuilder): Option[(String, List[CommonMarkAST])] =
      l match {
        case C(";") :: rest => Entities(buf.toString) map ((_, rest))
        case C(c) :: t if buf.isEmpty && c.head.isLetter || buf.nonEmpty && c.head.isLetterOrDigit =>
          buf ++= c
          parseName(t, buf)
        case _ => None
      }

    @tailrec
    def parseNumeric(l: List[CommonMarkAST],
                     digits: Char => Boolean,
                     base: Int,
                     buf: StringBuilder = new StringBuilder): Option[(String, List[CommonMarkAST])] =
      l match {
        case C(";") :: rest =>
          val ch =
            try {
              Integer.parseInt(buf.toString, base)
            } catch {
              case _: Exception => return None
            }

          Some(((if (ch > 0xFFFF || ch <= 0) 0xFFFD else ch).toChar.toString, rest))
        case C(c) :: t if digits(c.head) && buf.length < 7 =>
          buf ++= c
          parseNumeric(t, digits, base, buf)
        case _ => None
      }

    def parseHex(l: List[CommonMarkAST],
                 buf: StringBuilder = new StringBuilder): Option[(String, List[CommonMarkAST])] =
      parseNumeric(l, "0123456789abcdefABCDEF" contains _, 16)

    def parseDecimal(l: List[CommonMarkAST],
                     buf: StringBuilder = new StringBuilder): Option[(String, List[CommonMarkAST])] =
      parseNumeric(l, Character.isDigit, 10)

    buf ++= appends

    l match {
      case (c1 @ C("&")) :: (c2 @ C("#")) :: (c3 @ C("x" | "X")) :: t =>
        parseHex(t) match {
          case None              => entities(t, buf, c1, c2, c3)
          case Some((ent, rest)) => entities(rest, buf, Ce(ent))
        }
      case (c1 @ C("&")) :: (c2 @ C("#")) :: t =>
        parseDecimal(t) match {
          case None              => entities(t, buf, c1, c2)
          case Some((ent, rest)) => entities(rest, buf, Ce(ent))
        }
      case (c @ C("&")) :: t =>
        parseName(t) match {
          case None              => entities(t, buf, c)
          case Some((ent, rest)) => entities(rest, buf, Ce(ent))
        }
      case c :: t => entities(t, buf, c)
      case Nil    => buf.toList
    }
  }

  def escapes(s: String): List[CommonMarkAST] = entities(chars(s.toList), new ListBuffer)

  def escapedString(s: String): String = chars2string(escapes(s))

  def chars2string(cs: List[CommonMarkAST]): String = cs map (_.asInstanceOf[Chr].text) mkString

  def breaks(l: List[CommonMarkAST], buf: ListBuffer[CommonMarkAST] = new ListBuffer): List[CommonMarkAST] =
    l match {
      case Nil => buf.toList
      case C(" ") :: C(" ") :: t if t.span(_ != C("\n"))._1 forall (_ == C(" ")) =>
        buf += HardBreakAST
        breaks(t.span(_ != C("\n"))._2, buf)
      case C("\\") :: C("\n") :: t =>
        buf += HardBreakAST
        breaks(t, buf)
      case C(" ") :: C("\n") :: t =>
        buf += SoftBreakAST
        breaks(t, buf)
      case C("\n") :: t =>
        buf += SoftBreakAST
        breaks(t, buf)
      case e :: t =>
        buf += e
        breaks(t, buf)
    }

  private val punctuationCategories =
    Set(
      Character.CONNECTOR_PUNCTUATION,
      Character.DASH_PUNCTUATION,
      Character.END_PUNCTUATION,
      Character.FINAL_QUOTE_PUNCTUATION,
      Character.INITIAL_QUOTE_PUNCTUATION,
      Character.OTHER_PUNCTUATION,
      Character.START_PUNCTUATION
    )

  //  @tailrec
  //  def emph(l: List[CommonMarkAST],
  //           buf: ListBuffer[CommonMarkAST],
  //           body: ListBuffer[CommonMarkAST]): List[CommonMarkAST] =
  //    l match {
  //      case C("*") :: t if body == null => emph(t, buf, new ListBuffer)
  //      case C("*") :: t =>
  //        buf += EmphasisAST(TextAST(body.flatMap(_.asInstanceOf[LeafAST].text).mkString))
  //        emph(t, buf, null)
  //      case c :: t if body == null =>
  //        buf += c
  //        emph(t, buf, null)
  //      case c :: t =>
  //        body += c
  //        emph(t, buf, body)
  //      case Nil if body == null => buf.toList
  //      case Nil                 => (C("*") +: buf ++: body).toList
  //    }
  //
  //  emph( l, new ListBuffer, null)
  //  val res = emph(l, new ListBuffer, null)
  //  res

  //  def phase2(l: List[CommonMarkAST]): List[CommonMarkAST] = {
  //
  //    case class Delimiter(s: String, idx: Int, n: Int, opener: Boolean, closer: Boolean, var active: Boolean = true)
  //    case class TextNode(s: String)
  //
  //    val buf = new ListBuffer[CommonMarkAST]
  //    val stack = new DLList[Delimiter]
  //    var stack_bottom: stack.Node = null
  //
  //  }

  def phase2(l: List[CommonMarkAST]): List[CommonMarkAST] = {
    case class Delimiter(s: String,
                         var idx: Int,
                         var n: Int,
                         opener: Boolean,
                         closer: Boolean,
                         var active: Boolean = true)
    case class TextNode(s: String)

    val buf = new ListBuffer[CommonMarkAST]
    val stack = new DLList[Delimiter]
    val array = l to ArrayBuffer

    def punctuation(elem: CommonMarkAST) =
      elem match {
        case c: Chr =>
          val ch = c.text.head

          ("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~" contains ch) || punctuationCategories(Character.getType(ch).toByte)
        case _ => false
      }

    def whitespace(elem: CommonMarkAST) = {
      elem match {
        case c: Chr       => c.text.head.isWhitespace || c.text.head.isSpaceChar
        case SoftBreakAST => true
        case _            => false
      }
    }

    @tailrec
    def mark(c: C, idx: Int, f: C => Unit): Int =
      if (idx < array.length) {
        val o = array(idx)

        if (o == c) {
          f(o.asInstanceOf[C])
          mark(c, idx + 1, f)
        } else idx
      } else
        idx

    @tailrec
    def skip(c: C, idx: Int): Int =
      if (idx < array.length && array(idx) == c)
        skip(c, idx + 1)
      else
        idx

    def isFollowedByPunct(end: Int) = end < array.length && punctuation(array(end))

    def isPrecededByPunct(idx: Int) = idx > 0 && punctuation(array(idx - 1))

    def isFollowedBySpace(end: Int) = end == array.length || whitespace(array(end))

    def isPrecededBySpace(idx: Int) = idx == 0 || whitespace(array(idx - 1))

    @tailrec
    def flanking(idx: Int): Unit = {
      if (idx < array.length)
        array(idx) match {
          case c @ C("*" | "_") =>
            val end = skip(c, idx + 1)
            val followedByPunct = isFollowedByPunct(end)
            val precededByPunct = isPrecededByPunct(idx)
            val followedBySpace = isFollowedBySpace(end)
            val precededBySpace = isPrecededBySpace(idx)

            //            println("flanking", idx, c, precededBySpace, followedBySpace)
            if (!followedBySpace && (!followedByPunct || followedByPunct && (precededBySpace || precededByPunct)))
              mark(c, idx, x => {
                x.leftFlanking = true
                x.followedByPunct = followedByPunct
              })

            if (!precededBySpace && (!precededByPunct || precededByPunct && (followedBySpace || followedByPunct)))
              mark(c, idx, x => {
                x.rightFlanking = true
                x.precededByPunct = precededByPunct
              })

            flanking(end)
          case _ => flanking(idx + 1)
        }
    }

    def lookForLinkOrImage(endidx: Int): Unit = {
      val it = stack.reverseNodeIterator

      while (it.hasNext) {
        val node = it.next()

        node.element match {
          case Delimiter("[" | "![", idx, n, opener, closer, active) =>
            if (!active) {
              node.unlink
              array(endidx) = TextAST(array(endidx).asInstanceOf[C].text)
              return
            }

            val start = new CommonMarkArrayInput(array, idx)

            LinksImagesParser.run(start, linksImagesParser.pattern) match {
              case Some((Some(Link(text, url, title)), rest, _)) =>
                println(text, url, title)
              case Some((Some(Image(text, url, title)), rest, _)) =>
              case None =>
                array(endidx) = TextAST(array(endidx).asInstanceOf[C].text)
                return
            }
          case _ =>
        }
      }

      array(endidx) = TextAST(array(endidx).asInstanceOf[C].text)
    }

    @tailrec
    def delimiters(idx: Int): Unit =
      if (idx < array.length) {
        array(idx) match {
          case c @ C("*") if c.leftFlanking || c.rightFlanking =>
            val end = skip(c, idx)

            stack += Delimiter(c.text, idx, end - idx, c.leftFlanking, c.rightFlanking)
            delimiters(end)
          case c @ C("_") if c.leftFlanking || c.rightFlanking =>
            val end = skip(c, idx)

            stack += Delimiter(c.text,
                               idx,
                               end - idx,
                               c.leftFlanking && (!c.rightFlanking || c.precededByPunct),
                               c.rightFlanking && (!c.leftFlanking || c.followedByPunct))
            delimiters(end)
          case C("[") =>
            stack += Delimiter("[", idx, 1, opener = false, closer = false)
            delimiters(idx + 1)
          case C("!") if idx + 1 < array.length && array(idx + 1) == C("[") =>
            stack += Delimiter("![", idx, 2, opener = false, closer = false)
            delimiters(idx + 2)
          case C("]") => lookForLinkOrImage(idx)
          case _      => delimiters(idx + 1)
        }
      }

    flanking(0)
    delimiters(0)

//    var current_position: stack.Node = null

    def processEmphsis(stack_bottom: stack.Node = stack.startSentinel): Unit = {
      var current_position = stack_bottom.following

      case class Bottom(typ: String, closingLen: Int, closingCanBeOpener: Boolean)

      val openers_bottom = new mutable.HashMap[Bottom, stack.Node]

      for (typ <- List("*", "_"); closingLen <- 0 to 2; closingCanBeOpener <- List(false, true))
        openers_bottom(Bottom(typ, closingLen, closingCanBeOpener)) = stack_bottom

      while (!current_position.isAfterEnd) {
        while (!current_position.isAfterEnd && !current_position.element.closer) {
          current_position = current_position.following
        }

        if (!current_position.isAfterEnd) {
          var opener = current_position.preceding
          //          println("opener", opener)
          //          println("stack_bottom", stack_bottom)
          //          println(
          //            !opener.isBeforeStart && opener != stack_bottom && opener != openers_bottom(Bottom(
          //              current_position.element.s,
          //              current_position.element.n % 3,
          //              current_position.element.opener)) && !opener.element.opener)

          while (!opener.isBeforeStart && opener != stack_bottom && opener != openers_bottom(Bottom(
                   current_position.element.s,
                   current_position.element.n % 3,
                   current_position.element.opener)) &&
                 (!opener.element.opener || opener.element.s != current_position.element.s || (opener.element.closer || current_position.element.opener) &&
                 (opener.element.n + current_position.element.n) % 3 == 0 && opener.element.n % 3 != 0 && current_position.element.n % 3 != 0)) {
            opener = opener.preceding
            //            println(
            //              "looking back",
            //              opener,
            //              !opener.isBeforeStart && opener != stack_bottom && opener != openers_bottom(Bottom(
            //                current_position.element.s,
            //                current_position.element.n % 3,
            //                current_position.element.opener)) && !opener.element.opener
            //            )
          }

          //          println("---------")
          //          println("current_position", current_position)
          //          println("opener", opener)
          //          println("stack_bottom", stack_bottom)
          //          println("openers_bottom", openers_bottom)
          //          println("stack", stack)
          //          println(
          //            "if found",
          //            !opener.isBeforeStart && opener != stack_bottom && opener != openers_bottom(
          //              Bottom(current_position.element.s, current_position.element.n % 3, current_position.element.opener)) &&
          //              opener.element.s == current_position.element.s &&
          //              (opener.element.n + current_position.element.n) % 3 != 0
          //          )
          if (!opener.isBeforeStart && opener != stack_bottom && opener != openers_bottom(
                Bottom(current_position.element.s, current_position.element.n % 3, current_position.element.opener)) &&
              opener.element.s == current_position.element.s && !(current_position.element.opener && current_position.element.closer &&
                (opener.element.n + current_position.element.n) % 3 == 0 && opener.element.n % 3 != 0 && current_position.element.n % 3 != 0)) {
            //            println("+++ found +++")
            var d = opener.following

            while (d != current_position) {
              val next = d.following

              d.unlink
              d = next
            }

            val strong = opener.element.n >= 2 && current_position.element.n >= 2
            val body = SeqAST(
              textual(array.slice(opener.element.idx + opener.element.n, current_position.element.idx) toList))
            val tagged = current_position.element.idx - (opener.element.idx + opener.element.n)
            //            println("seq", body)
            //            println("array", array)

            array.remove(opener.element.idx + opener.element.n + 1,
                         current_position.element.idx - (opener.element.idx + opener.element.n + 1))
            array(opener.element.idx + opener.element.n) = if (strong) StrongAST(body) else EmphasisAST(body)

            var removed = if (strong) 2 else 1

            opener.element.n -= removed
            current_position.element.n -= removed
            array.remove(opener.element.idx, removed)
            current_position.element.idx -= removed + tagged - 1
            array.remove(current_position.element.idx, removed)

            d = current_position.following
            removed += tagged - 1 + removed

            while (!d.isAfterEnd) {
              d.element.idx -= removed
              d = d.following
            }

            //            println("stack", stack)

            if (opener.element.n == 0)
              opener.unlink

            if (current_position.element.n == 0) {
              val next = current_position.following

              current_position.unlink
              current_position = next
            }
          } else {
            //            println("--- not found ---")
            openers_bottom(
              Bottom(current_position.element.s, current_position.element.n % 3, current_position.element.opener)) =
              current_position.preceding

            if (!current_position.element.opener) {
              val next = current_position.following

              current_position.unlink
              current_position = next
            } else {
              current_position = current_position.following
            }
            //            println("current_position", current_position)
            //            println("openers_bottom", openers_bottom)
          }
        }
      }
    }

    processEmphsis()
    array.toList
  }

  def textual(l: List[CommonMarkAST], buf: ListBuffer[CommonMarkAST] = new ListBuffer): List[CommonMarkAST] =
    l match {
      case Nil => buf.toList
      case (_: Chr) :: _ =>
        val (cs, r) = l span (_.isInstanceOf[Chr])

        buf += TextAST(chars2string(cs))
        textual(r, buf)
      case e :: t =>
        buf += e
        textual(t, buf)
    }

  def inlineText(s: String): CommonMarkAST =
    textual(phase2(breaks(escapes(s)))) match {
      case List(e) => e
      case Nil     => SeqAST(Nil)
      case l       => SeqAST(if (l.last == HardBreakAST) l dropRight 1 else l)
    }

  def trim(s: String): String = {
    val buf = new StringBuilder(s)

    while (buf.nonEmpty && (buf.head == ' ' || buf.head == '\ue000')) buf.deleteCharAt(0)

    var i = buf.length - 1

    while (buf.nonEmpty && (buf.last == ' ' || buf.last == '\ue000')) buf.deleteCharAt(buf.length - 1)

    if (buf.length == s.length) s
    else buf.toString
  }

  def transform(s: LazyList[Block], loose: Boolean = true): List[CommonMarkAST] = {
    def blankAfter(s: collection.Seq[Block]) =
      if (s.length < 2)
        false
      else
        s.sliding(2).exists(a => a.head != BlankBlock && a.tail.head == BlankBlock)

    s match {
      case n if n isEmpty => Nil
      case h #:: t =>
        h match {
          case b: Block if !b.keep => transform(t, loose)
          case h: HTMLBlock        => HTMLBlockAST(h.buf.toString) :: transform(t, loose)
          case _: BreakBlock       => RuleAST :: transform(t, loose)
          case h: AHeadingBlock    => HeadingAST(h.level, inlineText(trim(h.heading)), None) :: transform(t, loose)
          case h: SHeadingBlock    => HeadingAST(h.level, inlineText(trim(h.heading)), None) :: transform(t, loose)
          case p: ParagraphBlock if loose =>
            ParagraphAST(inlineText(p.buf.toString)) :: transform(t, loose)
          case p: ParagraphBlock => inlineText(p.buf.toString) :: transform(t, loose)
          case b: IndentedBlock =>
            CodeBlockAST(b.buf.toString.lines.iterator.asScala.toList.reverse.dropWhile(isBlank).reverse mkString "\n",
                         None,
                         None) :: transform(t, loose)
          case f: FencedBlock =>
            CodeBlockAST(f.buf.toString, if (f.info nonEmpty) Some(escapedString(f.info)) else None, None) ::
              transform(t, loose)
          case q: QuoteBlock => BlockquoteAST(SeqAST(transform(q.blocks.to(LazyList)))) :: transform(t, loose)
          case l: ListItemBlock =>
            val (items, rest) = t span (b =>
              b.isInstanceOf[ListItemBlock] && b.asInstanceOf[ListItemBlock].typ == l.typ)
            val list = l +: items.asInstanceOf[LazyList[ListItemBlock]]
            val loose1 = list.init.exists(i => blankAfter(i.blocks)) || blankAfter(list.last.blocks.init)
            val listitems = list map (b => ListItemAST(SeqAST(transform(b.blocks.to(LazyList), loose1)))) toList
            val hd =
              if (l.typ.isInstanceOf[BulletList])
                BulletListAST(SeqAST(listitems), !loose1)
              else
                OrderedListAST(SeqAST(listitems), !loose1, l.typ.asInstanceOf[OrderedList].start)

            hd :: transform(rest, loose)
        }
    }
  }

}
