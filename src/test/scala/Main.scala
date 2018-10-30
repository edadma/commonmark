package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input = "<table><tr><td>\n<pre>\n**Hello**,\n\n_world_.\n</pre>\n</td></tr></table>\n"//"**foo*bar*baz**\n"
//    """
//      |foo*bar*
//    """.trim.stripMargin

  val doc = p.parse( input )

  println( p.parseBlocks(input.lines.toStream) )
  println( doc )
  println( Util.html(doc, 2) )
//  println( p.phase2(p.chars(input.toList)) )
}