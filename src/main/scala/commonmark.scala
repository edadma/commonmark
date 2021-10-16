package xyz.hyperreal


package object commonmark {

  def isBlank( s: String ) = s forall (_.isWhitespace)

  def nonBlank( s: String ) = !isBlank( s )

  def fromList( l: List[CommonMarkAST] ) = if (l.length == 1) l.head else SeqAST( l )

  def toList( e: CommonMarkAST ) =
    e match {
      case SeqAST( l ) => l
      case _ => List( e )
    }
}