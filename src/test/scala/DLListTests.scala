package xyz.hyperreal.commonmark

import org.scalatest._
import prop.PropertyChecks


class DLListTests extends FreeSpec with PropertyChecks with Matchers {

  "empty" in {
    val l = new DLList

    l.length shouldBe 0
    l.toString shouldBe "DLList()"
  }

}

//object DLListTest extends App {
//
//  val list = new DLList[Int]
//
//  list ++= Seq( 3, 4, 5 )
//  println( list.reverseIterator mkString ", " )
//
//}
