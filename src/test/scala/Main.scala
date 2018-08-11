package xyz.hyperreal.commonmark

import xyz.hyperreal.pretty.prettyPrint

object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |1. asdf
      |   qwer
      |
      |2. zxcv
    """.stripMargin

  val doc = p.parse( input )

  print( prettyPrint(doc) )
}