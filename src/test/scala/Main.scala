package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |# level 1 heading
      |this is a test
      |second line
      |- - -
      |zxcv
      |- - -
      |asdf
      |wret
    """.stripMargin

  val doc = p.parse( input )

  print( doc )
}