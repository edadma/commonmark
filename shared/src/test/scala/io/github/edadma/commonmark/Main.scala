package io.github.edadma.commonmark

import scala.jdk.CollectionConverters._

object Main extends App {

  val p = new CommonMarkParser
  //  val input = "__foo_ bar_" //407
  //  val input = "*foo**bar**baz*" //410
  //  val input = "*foo**bar*" //411
  val input = "foo *_*" //437 <p>foo <em>_</em></p>

  val doc = p.parse(input)

  //  println(p.parseBlocks(input.lines.iterator.asScala.to(LazyList)))
  //  println(doc)
  println(Util.html(doc, 2))
}
