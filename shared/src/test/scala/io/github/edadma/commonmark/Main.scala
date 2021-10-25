package io.github.edadma.commonmark

import pprint.pprintln

object Main extends App {

  val p = new CommonMarkParser

  val input =
    """
      |1
      |=
      |
      |body 1
      |
      |1.1
      |---
      |
      |body 1.1
      |
      |### 1.1.1
      |
      |body 1.1.1
      |
      |1.2
      |---
      |
      |body 1.2
      |
      |### 1.2.1
      |
      |body 1.2.1
      |""".stripMargin

  val doc = p.parse(input)

  //  println(p.parseBlocks(input.lines.iterator.asScala.to(LazyList)))
  //  println(doc)
  println(Util.html(doc, 2))
  pprintln(Util.toc(doc))
}
