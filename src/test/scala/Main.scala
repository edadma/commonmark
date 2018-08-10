package xyz.hyperreal.commonmark


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

  print( doc )
}