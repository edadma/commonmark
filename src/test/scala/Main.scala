package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
//    """
//      |> asdf
//      |zxcv
//      |poiu
//      |> qwer
//    """.stripMargin
    """
      |> 111
      |> > 222
      |> 333
      |444
      |> ---
    """.stripMargin

  val doc = p.parse( input )

  print( doc )
}