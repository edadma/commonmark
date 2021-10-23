package io.github.edadma.commonmark

import io.github.edadma.recognizer.CharRecognizer

object LinksImagesParser extends CharRecognizer[CommonMarkAST] {

  lazy val balancedDestination: Pattern = rep(noneOf('(', ')', ' ', '\n') | '(' ~ nonStrict(balancedDestination) ~ ')')
  lazy val balancedText: Pattern = rep(noneOf('[', ']') | '[' ~ nonStrict(balancedText) ~ ']')
  lazy val balancedText1: Pattern = rep1(noneOf('[', ']') | '[' ~ nonStrict(balancedText) ~ ']')
  val linkPattern: Pattern =
    '[' ~ string(balancedText) ~ ']' ~
      '(' ~ ws ~
      ('<' ~ string(rep(noneOf('>', '\n'))) ~ '>' | not('<') ~ string(balancedDestination)) ~
      opti(
        ws1 ~ ('"' ~ string(rep(noneOf('"'))) ~ '"' | '\'' ~ string(rep(noneOf('\''))) ~ '\'' | '(' ~ string(
          rep(noneOf(')'))) ~ ')')) ~ ws ~ ')' ~ action3(Link)
  val imagePattern: Pattern =
    "![" ~ string(balancedText) ~ ']' ~
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
    '[' ~ string(balancedText) ~ ']' ~ '[' ~ string(rep1(noneOf('[', ']'))) ~
      test(
        values =>
          values.nonEmpty && values.head.toString.exists(!_.isWhitespace) && parser.refs.contains(
            values.head.toString.toLowerCase)) ~ ']' ~ action2[String, String] { (t, l) =>
      val LinkInfo(url, title) = parser.refs(l.toLowerCase)

      Link(t, url, title)
    } |
      '[' ~ string(balancedText1) ~
        test(
          values =>
            values.nonEmpty && values.head.toString.exists(!_.isWhitespace) && parser.refs.contains(
              values.head.toString.toLowerCase)) ~ ']' ~
        opt("[]") ~ action[String] { s =>
        val LinkInfo(url, title) = parser.refs(s.toLowerCase)

        Link(s, url, title)
      }
  val refImagePattern: Pattern =
    "![" ~ string(balancedText) ~ ']' ~ '[' ~ string(rep1(noneOf('[', ']'))) ~
      test(
        values =>
          values.nonEmpty && values.head.toString.exists(!_.isWhitespace) && parser.refs.contains(
            values.head.toString.toLowerCase)) ~ ']' ~ action2[String, String] { (t, l) =>
      val LinkInfo(url, title) = parser.refs(l.toLowerCase)

      Image(t, url, title)
    } |
      "![" ~ string(balancedText1) ~
        test(
          values =>
            values.nonEmpty && values.head.toString.exists(!_.isWhitespace) && parser.refs.contains(
              values.head.toString.toLowerCase)) ~ ']' ~
        opt("[]") ~ action[String] { s =>
        val LinkInfo(url, title) = parser.refs(s.toLowerCase)

        Image(s, url, title)
      }
  val pattern: Pattern = linkPattern | refLinkPattern | imagePattern | refImagePattern

}
