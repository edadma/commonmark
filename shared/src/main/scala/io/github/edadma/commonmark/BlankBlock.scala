package io.github.edadma.commonmark

object BlankBlockType extends BlockType {

  override def start(from: Int,
                     text: String,
                     s: Stream[String],
                     prev: ContainerBlock,
                     parser: CommonMarkParser,
                     doc: DocumentBlock): Option[(Block, Int, String)] =
    if (isBlank(text) && !prev.open.exists(_.isInstanceOf[IndentedBlock]))
      Some((BlankBlock, from, text))
    else
      None

}

object BlankBlock extends SimpleLeafBlock { // this is an object and not a class for efficiency

  val name = "blank"

  keep = false

  def accept(from: Int, text: String, stream: Stream[String]): Option[(Int, String)] = // todo: do non appendable block need to code accept()
    if (isBlank(text))
      Some((from, text))
    else
      None

}
