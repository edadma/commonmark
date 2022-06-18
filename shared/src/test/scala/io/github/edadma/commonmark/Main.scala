package io.github.edadma.commonmark

import pprint.pprintln

object Main extends App {

  val p = new CommonMarkParser

  val input =
    """
      |[text1](/link1) | [text2](/link2) 
      |""".stripMargin

  val doc = p.parse(input)

  //  println(p.parseBlocks(input.lines.iterator.asScala.to(LazyList)))
  //  println(doc)
  println(Util.html(doc, 2))
//  pprintln(Util.toc(doc))
}
