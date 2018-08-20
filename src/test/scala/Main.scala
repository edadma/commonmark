package xyz.hyperreal.commonmark


object Main extends App {

  val l = new DLList

  println( l.reverseNodeIterator.next, l.reverseIterator.next )

  val p = new CommonMarkParser
  val input =
    """
      |foo <!-- not a comment -- two hyphens -->
    """.trim.stripMargin

  val doc = p.parse( input )

  println( p.parseBlocks( input.lines.toStream ))
  println( doc )
  println( Util.html(doc, 2) )
}