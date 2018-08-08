package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |wow
      |> poiu
      |> ====
      |> asdf
      |>     zxcv
      |>
      |>     qewr
      |>
      |>     lkjh
      |> - --
      |wee
    """.stripMargin

  val doc = p.parse( input )

  print( doc )
}