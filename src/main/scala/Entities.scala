package xyz.hyperreal.commonmark

import xyz.hyperreal.json.DefaultJSONReader


object Entities extends App {

  val map = {
    val json = DefaultJSONReader.fromFile( "entities.json" ).asInstanceOf[Map[String, Map[String, Any]]]

    json filter {case (k, v) => k endsWith ";"} map {case (k, v) => (k drop 1 dropRight 1) -> v("characters").toString}
  }

}