package io.github.edadma

package object commonmark {

  def isBlank(s: String) = s forall (c => c.isWhitespace || c == '\ue000')

  def nonBlank(s: String) = !isBlank(s)

}
