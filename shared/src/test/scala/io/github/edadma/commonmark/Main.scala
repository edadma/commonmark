package io.github.edadma.commonmark

object Main extends App {

  val p = new CommonMarkParser

  val input = "[link](/url)"

  val doc = p.parse(input)

  //  println(p.parseBlocks(input.lines.iterator.asScala.to(LazyList)))
  //  println(doc)
  println(Util.html(doc, 2))
}
