package io.github.edadma.commonmark

import scala.jdk.CollectionConverters._

trait Testing {

  def testBlockParsing(s: String) = (new CommonMarkParser).parseBlocks(s.lines.iterator().asScala.to(LazyList)).toString

  def test(s: String) = Util.html((new CommonMarkParser).parse(s), 2)

}
