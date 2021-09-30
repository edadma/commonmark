package xyz.hyperreal


package object commonmark {

  def isBlank( s: String ) = s forall (_.isWhitespace)

  def nonBlank( s: String ) = !isBlank( s )

}