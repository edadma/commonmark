//@
package io.github.edadma.commonmark

import java.util.regex.Matcher
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

case class Heading(heading: HeadingAST, sub: TOC)
case class TOC(headings: List[Heading])

object Util {

  private val urlchar: Set[Char] = ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ "-._~:/?#@!$&'()*+,;=" toSet
  private val blanksRegex = """\s+""".r
  private val blanksReplacement = Matcher.quoteReplacement("_")

  def toc(ast: CommonMarkAST): TOC = {
    val idmap = new mutable.LinkedHashMap[String, Int]

    def id(s: String) = {
      val ids =
        if (s.isEmpty) "_"
        else blanksRegex.replaceAllIn(s, blanksReplacement)

      idmap get ids match {
        case None =>
          idmap(ids) = 1
          ids
        case Some(count) =>
          val rid = s"$ids-$count"

          idmap(ids) = count + 1
          rid
      }
    }

    val stack = new mutable.Stack[ListBuffer[Heading]]

    def unnest(level: Int): Unit = {
      while (level < stack.top.last.heading.level && stack.length > 1) {
        val subs = stack.pop().toList

        stack.top(stack.top.length - 1) = stack.top.last.copy(sub = TOC(subs))
      }
    }

    def headingIds(ast: CommonMarkAST): Unit = {
      ast match {
        case SeqAST(s) => s foreach headingIds
        case h @ HeadingAST(level, contents, _) =>
          h.id = Some(id(text(contents)))

          if (stack.isEmpty || level > stack.top.last.heading.level) {
            stack push ListBuffer(Heading(h, TOC(Nil)))
          } else {
            unnest(level)
            stack.top += Heading(h, TOC(Nil))
          }
        case b: BranchAST => headingIds(b.contents)
        case _            =>
      }
    }

    headingIds(ast)

    if (stack.isEmpty) TOC(Nil)
    else {
      unnest(0)
      TOC(stack.top.toList)
    }
  }

  def html(doc: CommonMarkAST,
           tab: Int,
           codeblock: (String, Option[String], Option[String]) => String = null): String = {
    def attributes(attr: Seq[(String, String)]) =
      attr
        .filter { case ("align", "left") => false; case _ => true }
        .map { case (k, v) => s"""$k="${escape(v)}"""" }
        .mkString(" ") match {
        case "" => ""
        case s  => s" $s"
      }

    def nl(newline: Boolean) = if (newline) "\n" else ""

    def containerTag(tag: String, contents: CommonMarkAST, newline: Boolean, attr: (String, String)*) = {
      val h = html(contents).trim

      s"\n<$tag${attributes(attr)}>\n$h${if (h isEmpty) "" else "\n"}</$tag>\n"
    }

    def tag(tag: String, contents: CommonMarkAST, newline: Boolean, attr: (String, String)*) =
      s"${nl(newline)}<$tag${attributes(attr)}>${html(contents)}</$tag>${nl(newline)}"

    def optionalTag(tag: String, contents: CommonMarkAST, newline: Boolean, attr: (String, String)*) = {
      val c = html(contents)

      if (c nonEmpty)
        s"${nl(newline)}<$tag${attributes(attr)}>$c</$tag>${nl(newline)}"
      else
        ""
    }

    def leaf(tag: String, contents: String, attr: (String, String)*) =
      s"<$tag${attributes(attr)}>${escape(contents)}</$tag>"

    def escape(s: String) = {
      val buf = new StringBuilder

      s foreach {
        case '&' => buf ++= "&amp;"
        case '<' => buf ++= "&lt;"
        case '>' => buf ++= "&gt;"
        case '"' => buf ++= "&quot;"
        //        case '\\' => buf ++= "&bsol;"
        //        case '{' => buf ++= "&lcub;"
        //        case '}' => buf ++= "&rcub;"
        //        case c if c > '\u007F' => buf ++= s"&#${c.toInt};"
        case c =>
          buf += c
      }

      var i = 0

      while (i < buf.length) {
        if (buf(i) == '\ue000') {
          var count = 1

          while (i + count < buf.length && buf(i + count) == '\ue000') count += 1

          val tabs = count / 4 + (if (count % 4 > 0) 1 else 0)

          for (j <- i until i + tabs)
            buf(j) = '\t'

          i += tabs

          for (_ <- 1 to count - tabs)
            buf.deleteCharAt(i)
        } else
          i += 1
      }

      buf.toString
    }

    def encode(s: String) = {
      val buf = new StringBuilder

      for (c <- s)
        if (urlchar(c))
          buf += c
        else
          buf ++= (scala.io.Codec.toUTF8(c.toString) map (n => f"%%$n%02x".toUpperCase) mkString)

      buf.toString
    }

    def html(doc: CommonMarkAST): String =
      doc match {
        case SeqAST(seq) =>
          val buf = new StringBuilder

          for (s <- seq map html)
            buf ++= (if (buf.nonEmpty && buf.last == '\n' && s.startsWith("\n")) s drop 1 else s)

          buf.toString
        case URIAutolinkAST(addr) =>
          val escaped = escape(addr)
          val encoded = encode(escaped)

          s"""<a href="$encoded">$escaped</a>"""
        case EmailAutolinkAST(addr) =>
          val escaped = escape(addr)
          val encoded = encode(escaped)

          s"""<a href="mailto:$encoded">$escaped</a>"""
        case TextAST(t)                            => escape(t)
        case HTMLBlockAST(t)                       => s"\n$t\n"
        case RawHTMLAST(t)                         => t
        case ParagraphAST(contents)                => optionalTag("p", contents, true)
        case BlockquoteAST(contents)               => containerTag("blockquote", contents, true)
        case HeadingAST(level, contents, Some(id)) => tag(s"h$level", contents, true, "id" -> id)
        case HeadingAST(level, contents, None)     => tag(s"h$level", contents, true)
        case CodeSpanAST(c)                        => leaf("code", c)
        case CodeBlockAST(c, highlighted, caption) =>
          val escaped = escape(c) + (if (c isEmpty) "" else "\n")

          if (codeblock eq null)
            if (highlighted isDefined) {
              val clas = '"' +: s"language-${highlighted.get}" :+ '"'

              s"\n<pre><code class=$clas>$escaped</code></pre>\n"
            } else
              s"\n<pre><code>$escaped</code></pre>\n"
          else
            "\n" + codeblock(escaped, highlighted, caption) + "\n"
        case LinkAST(address, None, contents)         => tag("a", contents, false, "href" -> address)
        case ListItemAST(contents)                    => tag("li", contents, true)
        case BulletListAST(contents, tight)           => tag("ul", contents, true)
        case OrderedListAST(contents, tight, 1)       => tag("ol", contents, true)
        case OrderedListAST(contents, tight, start)   => tag("ol", contents, true, "start" -> start.toString)
        case LinkAST(address, Some(title), contents)  => tag("a", contents, false, "href" -> address, "title" -> title)
        case ImageAST(address, None, contents)        => tag("img", contents, false, "src" -> address)
        case ImageAST(address, Some(title), contents) => tag("img", contents, false, "src" -> address, "title" -> title)
        case EmphasisAST(contents)                    => tag("em", contents, false)
        case StrongAST(contents)                      => tag("strong", contents, false)
        case StrikethroughAST(contents)               => tag("del", contents, false)
        case SoftBreakAST                             => "\n"
        case HardBreakAST                             => "<br />\n"
        case RuleAST                                  => "\n<hr />\n"
        case TableHeadCellAST(align, contents)        => tag("th", contents, true, "align" -> align)
        case TableBodyCellAST(align, contents)        => tag("td", contents, false, "align" -> align)
        case TableRowAST(contents)                    => tag("tr", contents, true)
        case TableHeadAST(contents)                   => tag("thead", contents, true)
        case TableBodyAST(contents)                   => tag("tbody", contents, true)
        case TableAST(contents)                       => tag("table", contents, true)
        case EntityAST(entity, _)                     => s"&$entity;"
      }

    val h = html(doc) dropWhile (_ == '\n')

    if (h endsWith "\n")
      h
    else
      h + "\n"
  }

}
