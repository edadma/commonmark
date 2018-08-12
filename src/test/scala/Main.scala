package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |aaa
      |bbb
    """.stripMargin

  val doc = p.parse( input )

  println( Util.html(doc, 2) )
}