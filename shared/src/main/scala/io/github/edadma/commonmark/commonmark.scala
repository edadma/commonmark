package io.github.edadma

import scala.language.postfixOps

package object commonmark {

  def isBlank(s: String): Boolean = s forall (c => c.isWhitespace || c == '\ue000')

  def nonBlank(s: String): Boolean = !isBlank(s)

  def text(l: List[CommonMarkAST]): String = l map text mkString

  def text(n: CommonMarkAST): String = {
    n match {
      case leaf: LeafAST => leaf.text
      case e             => e.elements map text mkString
    }
  }

  case class LinkInfo(url: String, title: Option[String])
  case class Link(text: List[CommonMarkAST], url: String, title: Option[String])
  case class Image(text: List[CommonMarkAST], url: String, title: Option[String])

}
