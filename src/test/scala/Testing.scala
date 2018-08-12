package xyz.hyperreal.commonmark


trait Testing {

  def test( s: String ) = (new CommonMarkParser).parseBlocks( s.lines.toStream ).toString

}