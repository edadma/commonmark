package xyz.hyperreal.commonmark

import collection.mutable.AbstractBuffer


object DLListTest extends App {

  val list = new DLList[Int]

  list ++= Seq( 3, 4, 5 )
  println( list.reverseIterator mkString ", " )

}

class DLList[T] extends AbstractBuffer[T] {

  class Node( private [DLList] var prev: Node, private [DLList] var next: Node, var v: T ) {

    private [DLList] def this() = this( null, null, null.asInstanceOf[T] )

    def nextNode = next

    def prevNode = prev

    def isBeforeStart = eq( startSentinel )

    def isAfterEnd = eq( endSentinel )

    def unlink = {
      require( this ne startSentinel, "can't unlink the start sentinel" )
      require( this ne endSentinel, "can't unlink the end sentinel" )
      next.prev = prev
      prev.next = next
      prev = null
      next = null
      count -= 1
      v
    }

    def follow( v: T ) = {
      val node = new Node( this, next, v )

      next.prev = node
      next = node
      count += 1
      node
    }

    def precede( v: T ) = {
      val node = new Node( prev, this, v )

      prev.next = node
      prev = node
      count += 1
      node
    }

  }

  private val startSentinel = new Node
  private val endSentinel = new Node
  private var count = 0

  clear

  //
  // DLList operations
  //

  def appendElement( elem: T ) = endSentinel precede elem

  def headNode = {
    require( nonEmpty, "list is empty" )
    startSentinel.next
  }

  def lastNode = {
    require( nonEmpty, "list is empty" )
    endSentinel.prev
  }

  def node( n: Int ) = {
    require( 0 <= n && n < count, s"node index out of range: $n" )
    nodeIterator drop (n - 1) next
  }

  def nodeIterator =
    new Iterator[Node] {
      private var node = startSentinel

      def hasNext = node.next ne endSentinel

      def next = {
        if (!hasNext) throw new NoSuchElementException( "no more elements in list" )

        node = node.next
        node
      }
    }

  def reverseNodeIterator =
    new Iterator[Node] {
      private var node = endSentinel

      def hasNext = node.prev ne startSentinel

      def next = {
        if (!hasNext) throw new NoSuchElementException( "no more elements in list" )

        node = node.prev
        node
      }
    }

  def prependElement( elem: T ) = startSentinel follow elem

  //
  // abstract Buffer methods
  //

  def +=( elem: T ) = {
    appendElement( elem )
    this
  }

  def +=:( elem: T ) = {
    prependElement( elem )
    this
  }

  def apply( n: Int ) = node( n ).v

  def clear: Unit = {
    startSentinel.next = endSentinel
    endSentinel.prev = startSentinel
    count = 0
  }

  def iterator = nodeIterator map (_.v)

  def length = count

  def insertAll( n: Int, elems: Traversable[T] ) = {
    var prev = node( n )

    elems foreach (e => prev = prev follow e)
  }

  def remove( n: Int ) = node( n ).unlink

  def update( n: Int, newelem: T ): Unit = node( n ).v = newelem

  //
  // overrides
  //

  override def head = headNode.v

  override def last = lastNode.v

  override def reverseIterator = reverseNodeIterator map (_.v)

  override def toString = iterator mkString ("DLList(", ", ", ")")

}