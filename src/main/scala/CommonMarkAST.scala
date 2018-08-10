package xyz.hyperreal.commonmark


trait CommonMarkAST {
  def elements: Seq[CommonMarkAST]
}

case class SeqAST( seq: Seq[CommonMarkAST] ) extends CommonMarkAST {
  val contents = null

  override def elements = seq
}

trait BranchAST extends CommonMarkAST {
  val contents: CommonMarkAST

  def elements =
    contents match {
      case SeqAST( seq ) => seq
      case _ => Seq( contents )
    }
}

trait LeafAST extends CommonMarkAST {
  def elements = Nil

  val text: String
}

case class ParagraphAST( contents: CommonMarkAST ) extends BranchAST
case class BlockquoteAST( contents: CommonMarkAST ) extends BranchAST
case class HeadingAST( level: Int, contents: CommonMarkAST, var id: Option[String] = None ) extends BranchAST
case class CodeInlineAST( text: String ) extends LeafAST
case class CodeBlockAST( text: String, highlighted: Option[String], caption: Option[String] ) extends LeafAST
case class TextAST( text: String ) extends LeafAST
case class RawAST( text: String ) extends LeafAST
case class LinkAST( address: String, title: Option[String], contents: CommonMarkAST ) extends BranchAST
case class ListItemAST( contents: CommonMarkAST ) extends BranchAST
case class UnorderedListAST( contents: CommonMarkAST ) extends BranchAST
case class OrderedListAST( contents: CommonMarkAST ) extends BranchAST
case class ImageAST( address: String, title: Option[String], text: String ) extends LeafAST
case class EmphasisAST( contents: CommonMarkAST ) extends BranchAST
case class StrongAST( contents: CommonMarkAST ) extends BranchAST
case class StrikethroughAST( contents: CommonMarkAST ) extends BranchAST
case object BreakAST extends LeafAST { val text = "\n" }
case object RuleAST extends LeafAST { val text = "" }
case class TableHeadCellAST( align: String, contents: CommonMarkAST ) extends BranchAST
case class TableBodyCellAST( align: String, contents: CommonMarkAST ) extends BranchAST
case class TableRowAST( contents: CommonMarkAST ) extends BranchAST
case class TableHeadAST( contents: CommonMarkAST ) extends BranchAST
case class TableBodyAST(contents: CommonMarkAST ) extends BranchAST
case class TableAST( contents: CommonMarkAST ) extends BranchAST
case class EntityAST( entity: String, text: String ) extends LeafAST