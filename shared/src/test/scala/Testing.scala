package xyz.hyperreal.commonmark


trait Testing {

  def testBlockParsing( s: String ) = (new CommonMarkParser).parseBlocks( s.lines.toStream ).toString

  def test( s: String ) = Util.html( (new CommonMarkParser).parse(s), 2 )

}