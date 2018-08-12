package xyz.hyperreal.commonmark

import java.io.PrintWriter

import xyz.hyperreal.json.DefaultJSONReader


object SpecTestGenerator /*extends App*/ {

  val args = List[String]()
  val src = args(0)
  val sections = if (args(1) == "*") null else args(1) split "," toSet
  val spec_tests = DefaultJSONReader.fromFile( src ).asInstanceOf[List[Map[String, Any]]]

  for (section <- sections) {
    val name = section.replace(' ', '_') + "Tests"
    val out = new PrintWriter( name /*+ ".scala"*/ )

    out.println(
      s"""
        |package xyz.hyperreal.commonmark
        |
        |import org.scalatest._
        |import prop.PropertyChecks
        |
        |
        |class $name extends FreeSpec with PropertyChecks with Matchers with Testing {
      """.trim.stripMargin
    )
    out.println

    val tests = spec_tests filter (t => sections == null || section == t("section"))

    for (test <- tests) {
      out.println(
        s"""
          |  "example ${test("example")}" in {
          |    test( "${escape(test("markdown").toString)}" ) shouldBe "${escape(test("html").toString)}"
          |  }
        """.trim.stripMargin
      )
      out.println
    }

    out.println( "}" )
    out.close
  }

  def escape( s: String ) =
    s
      .replace( "\\", "\\\\" )
      .replace( "\t", "\\t" )
      .replace( "\n", "\\n" )

}