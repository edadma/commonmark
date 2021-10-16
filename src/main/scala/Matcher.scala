//@
package xyz.hyperreal.commonmark

import io.github.edadma.dllist.DLList

abstract class Result
case class Success(rest: Input) extends Result
case class Failure(pos: Input) extends Result

abstract class Input {

  def eoi: Boolean

  def more = !eoi

  def ch: Char

  def next: Input

  def group(name: String, start: Input, end: Input): Input

  def line: Int

  def col: Int

  def groups: Map[String, (Input, Input)]

  def substring(end: Input): String

  def lineText: String

  def errorText = lineText + '\n' + (" " * (col - 1)) + "^\n"

  def longErrorText =
    s"parser error on line $line, at column $col:\n" + errorText

  override def toString = s"line $line, col $col: $lineText; " +
    groups.map { case (k, (s, e)) => s"$k: ${s.substring(e)}" }.mkString(", ")

}

class StringInput private (
    s: String,
    val idx: Int,
    val line: Int,
    val col: Int,
    val groups: Map[String, (Input, Input)]
) extends Input {

  def this(s: String) = this(s, 0, 1, 1, Map())

  private def problem = sys.error(s"end of input: [$line, $col]")

  override lazy val eoi: Boolean = idx == s.length

  override lazy val ch: Char =
    if (eoi)
      problem
    else
      s(idx)

  override lazy val next: Input =
    if (eoi)
      problem
    else if (ch == '\n')
      new StringInput(s, idx + 1, line + 1, 1, groups)
    else
      new StringInput(s, idx + 1, line, col + 1, groups)

  def group(name: String, start: Input, end: Input) =
    new StringInput(s, idx, line, col, groups + (name -> (start, end)))

  def substring(end: Input) =
    s.substring(idx, end.asInstanceOf[StringInput].idx)

  override def lineText = {
    var ind = idx

    while (ind > 0 && s(ind - 1) != '\n')
      ind -= 1

    s.indexOf('\n', ind) match {
      case -1  => s substring ind
      case end => s.substring(ind, end)
    }
  }

}

trait Parser extends (Input => Result)

object Matcher extends App {

  val linkParser: Parser =
    seq(
      ch('['),
      zeroOrMore(noneOf(']')),
      ch(']'),
      ch('('),
      zeroOrMore(space),
      alt(
        seq(
          ch('<'),
          capture("dst", zeroOrMore(noneOf(')', '>', ' ', '\n'))),
          ch('>')
        ),
        seq(
          not(ch('<')),
          capture("dst", zeroOrMore(noneOf(')', '>', ' ', '\n'))),
          not(ch('>'))
        )
      ),
      opt(
        seq(
          oneOrMore(space),
          alt(
            seq(ch('"'), capture("title", zeroOrMore(noneOf('"'))), ch('"')),
            seq(ch('\''), capture("title", zeroOrMore(noneOf('\''))), ch('\'')),
            seq(ch('('), capture("title", zeroOrMore(noneOf(')'))), ch(')'))
          )
        )
      ),
      zeroOrMore(space),
      ch(')')
    )

  println(linkParser(new StringInput("""[asdf](</url>)""")))

  private val HEXDIGITSET = ('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9') toSet

  def hexdigit: Parser = cls(HEXDIGITSET)

  def letterOrDigit: Parser = cls(_.isLetterOrDigit)

  def letter: Parser = cls(_.isLetter)

  def lower: Parser = cls(_.isLower)

  def upper: Parser = cls(_.isUpper)

  def digit: Parser = cls(_.isDigit)

  def space: Parser = cls(_.isSpaceChar)

  def cls(pred: Char => Boolean)(in: Input): Result =
    if (in.more && pred(in.ch))
      Success(in.next)
    else
      Failure(in)

  def anyOf(cs: Char*): Parser = cls(cs contains _)

  def noneOf(cs: Char*): Parser = cls(!cs.contains(_))

  def oneOrMore(p: Parser): Parser = seq(p, zeroOrMore(p))

  def zeroOrMore(p: Parser)(in: Input): Result =
    p(in) match {
      case Success(r) => zeroOrMore(p)(r)
      case _          => Success(in)
    }

  def ch(c: Char): Parser = cls(_ == c)

  def str(s: String)(in: Input) = seq(s.toList map ch: _*)(in)

  def seq(ps: Parser*)(in: Input) = {
    def seq(idx: Int, r: Input): Result =
      if (idx < ps.length)
        ps(idx)(r) match {
          case Success(s) => seq(idx + 1, s)
          case f          => f
        }
      else
        Success(r)

    seq(0, in)
  }

  def alt(ps: Parser*)(in: Input) = {
    def alt(idx: Int): Result =
      if (idx < ps.length)
        ps(idx)(in) match {
          case s: Success => s
          case _          => alt(idx + 1)
        }
      else
        Failure(in)

    alt(0)
  }

  def capture(name: String, p: Parser)(in: Input) = {
    p(in) match {
      case Success(r) => Success(r.group(name, in, r))
      case f          => f
    }
  }

  def succeed(in: Input) = Success(in)

  def fail(in: Input) = Failure(in)

  def opt(p: Parser): Parser = alt(p, succeed)

  def print(a: Any): Parser = {
    println(a)
    succeed
  }

  def not(p: Parser)(in: Input) =
    p(in) match {
      case Success(_) => Failure(in)
      case Failure(_) => Success(in)
    }

}