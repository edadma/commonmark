package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |>     asdf
      |> qwer
      |zxcv
    """.stripMargin
//    """
//      |```
//      |asdf
//      |
//      |qwer
//      |```
//      |
//      |eryt
//    """.stripMargin
//    """
//      |    zxcv
//      |asdf
//    """.stripMargin
//    """
//      |poiu
//      |====
//      |asdf
//      |    zxcv
//      |
//      |    qewr
//      |
//      |    lkjh
//      |- --
//    """.stripMargin

  val doc = p.parse( input )

  print( doc )
}