//@
package xyz.hyperreal.commonmark

import scala.collection.mutable


object Util {

  def text( n: CommonMarkAST ): String = {
    n match {
      case leaf: LeafAST => leaf.text
      case e => e.elements map text mkString
    }
  }

  def headingIds( ast: CommonMarkAST ) = {
    val idmap = new mutable.HashMap[String, Int]
    val idset = new mutable.HashSet[String]

    def id( s: String ) = {
      val ids =
        if (s isEmpty)
          "_"
        else
          s.replace( ' ', '_' ).replace( '\t', '_' ).replace( '\r', '_' ).replace( '\n', '_' )

      if (idset(ids))
        idmap get ids match {
          case None =>
            val rid = s"$ids-1"

            idset += rid
            idmap(ids) = 2
            rid
          case Some( count ) =>
            val rid = s"$ids-$count"

            idset += rid
            idmap(ids) = count + 1
            rid
        }
      else {
        idset += ids
        idmap(ids) = 1
        ids
      }
    }

    def headingIds( ast: CommonMarkAST ): Unit = {
      ast match {
        case SeqAST( s ) => s foreach headingIds
        case h@HeadingAST( _, contents, _ ) => h.id = Some( id(text(contents)) )
        case b: BranchAST => headingIds( b.contents )
        case _ =>
      }
    }

    headingIds( ast )
  }

  def html( doc: CommonMarkAST, tab: Int, codeblock: (String, Option[String], Option[String]) => String = null ) = {
    val buf = new StringBuilder

    def attributes( attr: Seq[(String, String)] ) =
      attr.
        filter {case ("align", "left") => false; case _ => true}.
        map {case (k, v) => s"""$k="${escape( v )}""""}.
        mkString (" ") match {
          case "" => ""
          case s => s" $s"
        }

    def nl( newline: Boolean ) = if (newline) "\n" else ""

    def containerTag( tag: String, contents: CommonMarkAST, newline: Boolean, attr: (String, String)* ) = {
      val h = html( contents ).trim

      s"\n<$tag${attributes( attr )}>\n$h${if (h isEmpty) "" else "\n"}</$tag>\n"
    }

    def tag( tag: String, contents: CommonMarkAST, newline: Boolean, attr: (String, String)* ) =
      s"${nl(newline)}<$tag${attributes( attr )}>${html( contents )}</$tag>${nl(newline)}"

    def optionalTag( tag: String, contents: CommonMarkAST, newline: Boolean, attr: (String, String)* ) = {
      val c = html( contents )

      if (c nonEmpty)
        s"${nl(newline)}<$tag${attributes( attr )}>$c</$tag>${nl(newline)}"
      else
        ""
    }

    def leaf( tag: String, contents: String, attr: (String, String)* ) =
      s"<$tag${attributes( attr )}>${escape( contents )}</$tag>"

    def escape( s: String ) = {
      val buf = new StringBuilder

      s foreach {
        case '&' => buf ++= "&amp;"
        case '<' => buf ++= "&lt;"
        case '>' => buf ++= "&gt;"
        case '"' => buf ++= "&quot;"
//        case '\\' => buf ++= "&bsol;"
//        case '{' => buf ++= "&lcub;"
//        case '}' => buf ++= "&rcub;"
        case c if c > '\u007F' => buf ++= s"&#${c.toInt};"
        case c => buf += c
      }

      buf.toString
    }

    def html( doc: CommonMarkAST ): String =
      doc match {
        case SeqAST( seq ) =>
          val buf = new StringBuilder

          for (s <- seq map html)
            buf ++= (if (buf.nonEmpty && buf.last == '\n' && s.startsWith("\n")) (s drop 1) else s)

          buf.toString
        case TextAST( t ) => escape( t )
        case RawAST( t ) => t
        case ParagraphAST( contents ) => optionalTag( "p", contents, true )
        case BlockquoteAST( contents ) => containerTag( "blockquote", contents, true )
        case HeadingAST( level, contents, Some(id) ) => tag( s"h$level", contents, true, "id" -> id )
        case HeadingAST( level, contents, None ) => tag( s"h$level", contents, true )
        case CodeInlineAST( c ) => leaf( "code", c )
        case CodeBlockAST( c, highlighted, caption ) =>
          val escaped = escape( c ) + (if (c isEmpty) "" else "\n")

          if (codeblock eq null)
            if (highlighted isDefined) {
              val clas = '"' + s"language-${highlighted.get}" + '"'

              s"\n<pre><code class=$clas>$escaped</code></pre>\n"
            } else
              s"\n<pre><code>$escaped</code></pre>\n"
          else
            "\n" + codeblock( escaped, highlighted, caption ) + "\n"
        case LinkAST( address, None, contents ) => tag( "a", contents, false, "href" -> address )
        case LinkAST( address, Some(title), contents ) => tag( "a", contents, false, "href" -> address, "title" -> title )
        case ListItemAST( contents ) => tag( "li", contents, true )
        case BulletListAST( contents, tight ) => tag( "ul", contents, true )
        case OrderedListAST( contents, tight ) => tag( "ol", contents, true )
        case ImageAST( address, None, text ) => leaf( "img", text, "src" -> address )
        case ImageAST( address, Some(title), text ) => leaf( "img", text, "src" -> address, "title" -> title )
        case EmphasisAST( contents ) => tag( "em", contents, false )
        case StrongAST( contents ) => tag( "strong", contents, false )
        case StrikethroughAST( contents ) => tag( "del", contents, false )
        case SoftBreakAST => "\n"
        case HardBreakAST => "<br />\n"
        case RuleAST => "\n<hr />\n"
        case TableHeadCellAST( align, contents ) => tag( "th", contents, true, "align" -> align )
        case TableBodyCellAST( align, contents ) => tag( "td", contents, false, "align" -> align )
        case TableRowAST( contents ) => tag( "tr", contents, true )
        case TableHeadAST( contents ) => tag( "thead", contents, true )
        case TableBodyAST( contents ) => tag( "tbody", contents, true )
        case TableAST( contents ) => tag( "table", contents, true )
        case EntityAST( entity, _ ) => s"&$entity;"
      }

    html( doc ).trim + "\n"
  }

}