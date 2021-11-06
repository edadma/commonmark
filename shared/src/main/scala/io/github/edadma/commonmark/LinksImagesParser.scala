package io.github.edadma.commonmark

import io.github.edadma.recognizer.CharRecognizer

object LinksImagesParser extends CharRecognizer[CommonMarkAST] {

  lazy val balancedDestination: Pattern = rep(noneOf('(', ')', ' ', '\n') | '(' ~ nonStrict(balancedDestination) ~ ')')
  lazy val balancedText: Pattern = rep(noneOf('[', ']') | '[' ~ nonStrict(balancedText) ~ ']')
  lazy val balancedText1: Pattern = rep1(noneOf('[', ']') | '[' ~ nonStrict(balancedText) ~ ']')
  val linkPattern: Pattern =
    '[' ~ captureWrapped(balancedText) ~ ']' ~
      '(' ~ ws ~
      ('<' ~ string(rep(noneOf('>', '\n'))) ~ '>' | not('<') ~ string(balancedDestination)) ~
      opti(
        ws1 ~ ('"' ~ string(rep(noneOf('"'))) ~ '"' | '\'' ~ string(rep(noneOf('\''))) ~ '\'' | '(' ~ string(
          rep(noneOf(')'))) ~ ')')) ~ ws ~ ')' ~ action3(Link)
  val imagePattern: Pattern =
    "![" ~ captureWrapped(balancedText) ~ ']' ~
      '(' ~ ws ~
      ('<' ~ string(rep(noneOf('>', '\n'))) ~ '>' | not('<') ~ string(balancedDestination)) ~
      opti(
        ws1 ~ ('"' ~ string(rep(noneOf('"'))) ~ '"' | '\'' ~ string(rep(noneOf('\''))) ~ '\'' | '(' ~ string(
          rep(noneOf(')'))) ~ ')')) ~ ws ~ ')' ~ action3(Image)

}

class LinksImagesParser(parser: CommonMarkParser) {

  import LinksImagesParser._

//  runlimit = 50

  val refLinkPattern: Pattern =
    '[' ~ captureWrapped(balancedText) ~ ']' ~ '[' ~ captureWrapped(rep1(noneOf('[', ']'))) ~
      testValues(
        values =>
          values.nonEmpty && values.head.toString.exists(!_.isWhitespace) && parser.refs.contains(
            values.head.toString.toLowerCase)) ~ ']' ~ action2[List[CommonMarkAST], String] { (t, l) =>
      val LinkInfo(url, title) = parser.refs(l.toLowerCase)

      Link(t, url, title)
    } |
      '[' ~ captureWrapped(balancedText1) ~ action[List[CommonMarkAST]](l => (l, text(l).toLowerCase)) ~
        test[(List[CommonMarkAST], String)](t => t._2.exists(!_.isWhitespace) && parser.refs.contains(t._2)) ~ ']' ~
        opt("[]") ~ action[(List[CommonMarkAST], String)] { t =>
        val LinkInfo(url, title) = parser.refs(t._2)

        Link(t._1, url, title)
      }
  val refImagePattern: Pattern =
    "![" ~ captureWrapped(balancedText) ~ ']' ~ '[' ~ string(rep1(noneOf('[', ']'))) ~
      testValues(
        values =>
          values.nonEmpty && values.head.toString.exists(!_.isWhitespace) && parser.refs.contains(
            values.head.toString.toLowerCase)) ~ ']' ~ action2[List[CommonMarkAST], String] { (t, l) =>
      val LinkInfo(url, title) = parser.refs(l.toLowerCase)

      Image(t, url, title)
    } |
      "![" ~ captureWrapped(balancedText1) ~ action[List[CommonMarkAST]](l => (l, text(l).toLowerCase)) ~
        test[(List[CommonMarkAST], String)](t => t._2.exists(!_.isWhitespace) && parser.refs.contains(t._2)) ~ ']' ~
        opt("[]") ~ action[(List[CommonMarkAST], String)] { t =>
        val LinkInfo(url, title) = parser.refs(t._2)

        Image(t._1, url, title)
      }
  val pattern: Pattern = linkPattern | refLinkPattern | imagePattern | refImagePattern

}
