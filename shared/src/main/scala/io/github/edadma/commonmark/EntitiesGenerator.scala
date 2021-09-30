package io.github.edadma.commonmark

import java.io.PrintWriter

import io.github.edadma.json.DefaultJSONReader

object EntitiesGenerator extends App {

  val json = DefaultJSONReader.fromFile("entities.json").asInstanceOf[Map[String, Map[String, Any]]]
  val out = new PrintWriter("Entities")

  out.println(
    """
      |package io.github.edadma.commonmark
      |
      |
      |object Entities {
      |
      |  val map =
      |    Map(
    """.trim.stripMargin
  )
  out.println(
    json
      .filter { case (k, v) => k endsWith ";" }
      .map { case (k, v) => (k drop 1 dropRight 1) -> v("characters").toString }
      .toList
      .sorted
      .map { case (k, v) => s"""      "$k" -> "${v map (c => "\\u" + f"$c%04x") mkString}"""" }
      .mkString(",\n")
  )
  out.println("    )")
  out.println("}")
  out.close

}
