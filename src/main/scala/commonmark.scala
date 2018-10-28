package xyz.hyperreal


package object commonmark {

  def isBlank( s: String ) = s forall (_.isWhitespace)

  def nonBlank( s: String ) = !isBlank( s )

  def seq( l: Seq[CommonMarkAST] ) = if (l.length == 1) l.head else SeqAST( l )

}