package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |1.     indented code
      |
      |   paragraph
      |
      |       more code
    """.trim.stripMargin

  val doc = p.parse( input )

  println( p.parseBlocks( input.lines.toStream ))
  println( Util.html(doc, 2) )
}