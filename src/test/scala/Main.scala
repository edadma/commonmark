package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |foo <!-- this is a
      |comment - with hyphen -->
    """.trim.stripMargin

  val doc = p.parse( input )

  println( p.parseBlocks( input.lines.toStream ))
  println( doc )
  println( Util.html(doc, 2) )
}