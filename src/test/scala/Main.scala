package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |heading
      |=======
      |asdf
      |- --
      |# lkjh
      |    asdf
      |
      |    # qwer
    """.stripMargin
//    """
//      |poiu
//      |====
//      |asdf
//      |    zxcv
//      |
//      |    qewr
//      |
//      |    lkjh
//    """.stripMargin

  val doc = p.parse( input )

  print( doc )
}