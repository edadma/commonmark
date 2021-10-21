package io.github.edadma

package object commonmark {

  def isBlank(s: String) = s forall (c => c.isWhitespace || c == '\ue000')

  def nonBlank(s: String) = !isBlank(s)

  case class LinkInfo(url: String, title: Option[String])
  case class Link(text: String, url: String, title: Option[String])
  case class Image(text: String, url: String, title: Option[String])

}
