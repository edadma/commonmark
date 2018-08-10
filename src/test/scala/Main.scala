package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |[asdf]: \qwer
      |asdf
    """.stripMargin

  val doc = p.parse( input )

  print( doc )
}