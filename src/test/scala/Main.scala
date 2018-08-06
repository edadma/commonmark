package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |    a
    """.stripMargin

  val doc = p.parse( input )

  print( doc )
}