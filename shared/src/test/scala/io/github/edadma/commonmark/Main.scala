package io.github.edadma.commonmark

object Main extends App {

  val p = new CommonMarkParser

  val input =
    """
      |H1
      |==
      |
      |after h1
      |
      |H2
      |--
      |
      |after h2
      |
      |### H3
      |
      |after h3""".stripMargin

  val doc = p.parse(input)

  //  println(p.parseBlocks(input.lines.iterator.asScala.to(LazyList)))
  //  println(doc)
  println(Util.html(doc, 2))
  println(Util.headingIds(doc))
}
