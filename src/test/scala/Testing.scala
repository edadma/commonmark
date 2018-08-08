package xyz.hyperreal.commonmark


trait Testing {

  def test( s: String ) = (new CommonMarkParser).parse( s ).toString

}