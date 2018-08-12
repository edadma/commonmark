//@
package xyz.hyperreal.commonmark

import java.io.PrintWriter

import xyz.hyperreal.json.DefaultJSONReader


object SpecTestGenerator /*extends App*/ {

  val args = List[String]()
  val src = args(0)
  val spec_tests = DefaultJSONReader.fromFile( src ).asInstanceOf[List[Map[String, Any]]]

  if (args.length == 1)
    println(
      (spec_tests map (t => t("section").toString)).toSet.toList.sorted mkString "\n"
    )
  else {
    val dst = args(1)
    val sections = if (args(2) == "*") null else args(2) split "," toSet

    for (section <- sections) {
      val name = s"${section.replace(' ', '_')}SpecTests"
      val out = new PrintWriter( s"$dst/$name.scala" )

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

}