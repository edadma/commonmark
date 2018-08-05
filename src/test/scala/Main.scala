package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |this is a test
      |second line
    """.trim.stripMargin

  val doc = p.parse( input )

  print( doc )
}