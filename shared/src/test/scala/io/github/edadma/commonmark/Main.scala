package io.github.edadma.commonmark

import scala.jdk.CollectionConverters._

object Main extends App {

  val p = new CommonMarkParser
  val input = s"*${160.toChar}a${160.toChar}*"

  val doc = p.parse(input)

  //  println(p.parseBlocks(input.lines.iterator.asScala.to(LazyList)))
  //  println(doc)
  println(Util.html(doc, 2))
}
