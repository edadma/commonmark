package xyz.hyperreal.commonmark

import xyz.hyperreal.pretty.prettyPrint

object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |- item
      |
      |      zxcv
    """.stripMargin

  val doc = p.parse( input )

  print( prettyPrint(doc) )
}