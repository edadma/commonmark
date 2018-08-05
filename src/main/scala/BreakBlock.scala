package xyz.hyperreal.commonmark


object BreakBlockType extends BlockType {

  val breakRegex = """[ ]{0,3}([+_*])\s*(?:\1\s*){2,}"""r

  override def start( from: Int, s: Stream[String] ) =
    if (nonBlank( from, s ))
      Some( new BreakBlock )
    else
      None

}

class BreakBlock extends SimpleLeafBlock {

  val name = "break"

  def accept( from: Int, stream: Stream[String] ) =
    if (nonBlank( from, stream))
      Some( from )
    else
      None

}