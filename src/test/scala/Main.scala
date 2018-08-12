package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |> foo
      |-----
    """.stripMargin

  val doc = p.parse( input )

  println( doc)
  println( Util.html(doc, 2) )
}