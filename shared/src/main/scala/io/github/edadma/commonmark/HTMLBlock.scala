//@
package io.github.edadma.commonmark

import scala.language.postfixOps

object HTMLBlockType extends BlockType {

  val start1Regex = """(?i)[ ]{0,3}(?:<script|<pre|<style)\s*>?""" r
  val start2Regex = """[ ]{0,3}<!--.*""" r
  val start3Regex = """[ ]{0,3}<\?.*""" r
  val end1Regex = """.*(?:</script>|</pre>|</style>).*""" r
  val start4Regex = """[ ]{0,3}<![A-Z].*""" r
  val start5Regex = """[ ]{0,3}<!\[CDATA\[.*""" r
  val start6Regex =
    """(?i)[ ]{0,3}</?(?:address|article|aside|base|basefont|blockquote|body|caption|center|col|colgroup|dd|details|dialog|dir|div|dl|dt|fieldset|figcaption|figure|footer|form|frame|frameset|h1|h2|h3|h4|h5|h6|head|header|hr|html|iframe|legend|li|link|main|menu|menuitem|meta|nav|noframes|ol|optgroup|option|p|param|section|source|summary|table|tbody|td|tfoot|th|thead|title|tr|track|ul).*""" r
  val start7Regex =
    """(?i)<(?:[a-z][a-z0-9-]*(?:\s+[a-z_:][a-z90-9_.:-]*(?:\s*=\s*(?:[^ "'=<>`]+|'[^']*'|"[^"]*"))?)*\s*/?|/[a-z][a-z0-9-]*\s*)>""" r

  val starts =
    List(
      (from: Int, text: String, s: LazyList[String]) => start1Regex.pattern.matcher(text).matches,
      (from: Int, text: String, s: LazyList[String]) => start2Regex.pattern.matcher(text).matches,
      (from: Int, text: String, s: LazyList[String]) => start3Regex.pattern.matcher(text).matches,
      (from: Int, text: String, s: LazyList[String]) => start4Regex.pattern.matcher(text).matches,
      (from: Int, text: String, s: LazyList[String]) => start5Regex.pattern.matcher(text).matches,
      (from: Int, text: String, s: LazyList[String]) => start6Regex.pattern.matcher(text).matches,
      (from: Int, text: String, s: LazyList[String]) => start7Regex.pattern.matcher(text).matches
    )
  val ends =
    Vector(
      (from: Int, text: String, s: LazyList[String]) => HTMLBlockType.end1Regex.pattern.matcher(text).matches,
      (from: Int, text: String, s: LazyList[String]) => text contains "-->",
      (from: Int, text: String, s: LazyList[String]) => text contains "?>",
      (from: Int, text: String, s: LazyList[String]) => text contains ">",
      (from: Int, text: String, s: LazyList[String]) => text contains "]]>",
      (from: Int, text: String, s: LazyList[String]) =>
        s.isEmpty || s.tail.isEmpty || s.tail.head.length <= from || isBlank(s.tail.head substring from),
      (from: Int, text: String, s: LazyList[String]) =>
        s.isEmpty || s.tail.isEmpty || s.tail.head.length <= from || isBlank(s.tail.head substring from)
    )

  override def start(from: Int,
                     text: String,
                     s: LazyList[String],
                     prev: ContainerBlock,
                     parser: CommonMarkParser,
                     doc: DocumentBlock): Option[(Block, Int, String)] = {
    for ((st, i) <- starts zipWithIndex)
      if (st(from, text, s) && !(i == 6 && prev.open.exists(_.isInstanceOf[ParagraphBlock]))) {
        if (end(i, from, text, s))
          return Some((new HTMLBlock(i, true), from, text))
        else
          return Some((new HTMLBlock(i, false), from, text))
      }

    None
  }

  def end(ind: Int, from: Int, text: String, stream: LazyList[String]) = ends(ind)(from, text, stream)

}

class HTMLBlock(val typ: Int, var ended: Boolean) extends TextLeafBlock {

  val name = "html"

  override val isInterruptible = false

  def accept(from: Int, text: String, stream: LazyList[String]): Option[(Int, String)] = {
    if (ended)
      None
    else {
      if (HTMLBlockType.end(typ, from, text, stream))
        ended = true

      Some((from, text))
    }
  }

}
