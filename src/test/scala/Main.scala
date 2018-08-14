package xyz.hyperreal.commonmark


object Main extends App {

  val p = new CommonMarkParser
  val input =
    """
      |<table>
      |
      |  <tr>
      |
      |    <td>
      |      Hi
      |    </td>
      |
      |  </tr>
      |
      |</table>
    """.trim.stripMargin

  val doc = p.parse( input )

  println( p.parseBlocks( input.lines.toStream ))
  println( Util.html(doc, 2) )
}