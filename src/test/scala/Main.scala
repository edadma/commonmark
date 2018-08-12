package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |   > # Foo
      |   > bar
      | > baz
    """.stripMargin

  val doc = p.parse( input )

  println( Util.html(doc, 2) )
}