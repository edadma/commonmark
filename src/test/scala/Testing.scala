package xyz.hyperreal.commonmark


trait Testing {

  def testBlockParsing( s: String ) = (new CommonMarkParser).parseBlocks( LazyList.from(s.split("\n")) ).toString

  def test( s: String ) = Util.html( (new CommonMarkParser).parse(s), 2 )

}