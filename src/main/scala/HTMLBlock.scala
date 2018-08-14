//@
package xyz.hyperreal.commonmark


object HTMLBlockType extends BlockType {

  val start1Regex = """(?i)(?:<script|<pre|<style)\s*>?"""r
  val end1Regex = """.*(?:</script>|</pre>|</style>).*"""r
  val start4Regex = """<![A-Z]"""r
  val start5Regex = """(?i)</?(?:address|article|aside|base|basefont|blockquote|body|caption|center|col|colgroup|dd|details|dialog|dir|div|dl|dt|fieldset|figcaption|figure|footer|form|frame|frameset|h1|h2|h3|h4|h5|h6|head|header|hr|html|iframe|legend|li|link|main|menu|menuitem|meta|nav|noframes|ol|optgroup|option|p|param|section|source|summary|table|tbody|td|tfoot|th|thead|title|tr|track|ul)\s*(?:/?>)?"""r
  val starts =
    List(
      (from: Int, text: String, s: Stream[String]) => start1Regex.pattern.matcher( text ).matches,
      (from: Int, text: String, s: Stream[String]) => text startsWith "<!--",
      (from: Int, text: String, s: Stream[String]) => text startsWith "<?",
      (from: Int, text: String, s: Stream[String]) => start4Regex.pattern.matcher( text ).matches,
      (from: Int, text: String, s: Stream[String]) => start5Regex.pattern.matcher( text ).matches
    )
  val ends =
    Vector(
      (from: Int, text: String, s: Stream[String]) => HTMLBlockType.end1Regex.pattern.matcher( text ).matches,
      (from: Int, text: String, s: Stream[String]) => text contains "-->",
      (from: Int, text: String, s: Stream[String]) => text contains "?>",
      (from: Int, text: String, s: Stream[String]) => text contains ">",
      (from: Int, text: String, s: Stream[String]) => s.isEmpty || isBlank( s.tail.head substring from )
    )

  override def start(from: Int, text: String, s: Stream[String], prev: ContainerBlock, parser: CommonMarkParser, doc: DocumentBlock): Option[(Block, Int, String)] = {
    for ((st, i) <- starts zipWithIndex)
      if (st( from, text, s ))
        if (end(i, from, text, s ))
          return Some( (new HTMLBlock(None), from, text) )
        else
          return Some( (new HTMLBlock(Some(i)), from, text) )

    None
  }

  def end( ind: Int, from: Int, text: String, stream: Stream[String]) = ends(ind)( from, text, stream )

}

class HTMLBlock( var cond: Option[Int] ) extends TextLeafBlock {

  val name = "html"

  override val isInterruptible = false

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)] = {
    cond match {
      case None => None
      case Some( ind ) =>
        if (HTMLBlockType.end( ind, from, text, stream ))
          cond = None

        Some( (from, text) )
    }
  }

}