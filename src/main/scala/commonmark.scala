package xyz.hyperreal


package object commonmark {

  def isBlank( from: Int, s: Stream[String] ) = s.head drop from forall (_.isWhitespace)

  def nonBlank( from: Int, s: Stream[String] ) = !isBlank( from, s )

}