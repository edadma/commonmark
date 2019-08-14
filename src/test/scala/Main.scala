package xyz.hyperreal.commonmark

import scala.jdk.CollectionConverters._


object Main extends App {

  val p = new CommonMarkParser
  val input = "*[asdf](/url)*"
//    """
//      |foo*bar*
//    """.trim.stripMargin

  val doc = p.parse( input )

  println( p.parseBlocks(LazyList.from(input.split("\n"))) )
  println( doc )
  println( Util.html(doc, 2) )
//  println( p.phase2(p.chars(input.toList)) )
}