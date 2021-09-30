package io.github.edadma.commonmark

object Entities {

  def apply(s: String) = map get s

  val map =
    Map(
      "nbsp" -> "\u00a0",
      "amp" -> "\u0026",
      "copy" -> "\u00a9",
      "AElig" -> "\u00c6",
      "Dcaron" -> "\u010e",
      "frac34" -> "\u00be",
      "HilbertSpace" -> "\u210b",
      "DifferentialD" -> "\u2146",
      "ClockwiseContourIntegral" -> "\u2232",
      "ngE" -> "\u2267\u0338",
      "ouml" -> "\u00F6"
    )

}
