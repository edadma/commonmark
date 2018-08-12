package xyz.hyperreal.commonmark

import xyz.hyperreal.pretty.prettyPrint

object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |-     item
      |  asdf
      |- zxcv
    """.stripMargin

  val doc = p.parse( input )

  println( Util.html(doc, 2) )
}