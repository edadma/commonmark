package io.github.edadma.commonmark

import scala.jdk.CollectionConverters._

object Main extends App {

  val p = new CommonMarkParser
  //  val input = "__foo_ bar_" //407
  //  val input = "*foo**bar**baz*" //410
  //  val input = "*foo**bar*" //411

  val input = "  Foo *bar\nbaz*\t\n====\n"
  //"<h1>Foo <em>bar\nbaz</em></h1>\n"

  //  val input = "<table><tr><td>\n<pre>\n**Hello**,\n\n_world_.\n</pre>\n</td></tr></table>\n"
  //"<table><tr><td>\n<pre>\n**Hello**,\n<p><em>world</em>.\n</pre></p>\n</td></tr></table>\n"

  val doc = p.parse(input)

  //  println(p.parseBlocks(input.lines.iterator.asScala.to(LazyList)))
  //  println(doc)
  println(Util.html(doc, 2))
}
