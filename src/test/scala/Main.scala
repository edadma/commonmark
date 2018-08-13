package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |1.  A paragraph
      |    with two lines.
      |
      |        indented code
      |
      |    > A block quote.
    """.trim.stripMargin

  val doc = p.parse( input )

//  println( p.parseBlocks( input.lines.toStream ))
  println( Util.html(doc, 2) )
}