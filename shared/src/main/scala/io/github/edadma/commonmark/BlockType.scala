package io.github.edadma.commonmark

abstract class BlockType {

  val tab = "\ue000"

  implicit class tabstring(s: String) {
    def t: String = s.replace("tab", tab)
  }

  def start(from: Int,
            text: String,
            s: LazyList[String],
            prev: ContainerBlock,
            parser: CommonMarkParser,
            doc: DocumentBlock): Option[(Block, Int, String)]

}
