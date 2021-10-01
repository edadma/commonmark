package io.github.edadma.commonmark

import collection.mutable.AbstractBuffer
import scala.collection.mutable

object DLList {

  def apply[T](elems: T*) =
    new DLList[T] {
      appendAll(elems)
    }

}

class DLList[T] extends mutable.AbstractBuffer[T] {

  class Node(private[DLList] var prev: Node, private[DLList] var next: Node, init: T) {

    private[DLList] def this() = this(null, null, null.asInstanceOf[T])

    private[DLList] var v = init

    def element: T = v

    def element_=(value: T): Unit = v = value

    def following: Node = next

    def preceding: Node = prev

    def isBeforeStart = eq(startSentinel)

    def isAfterEnd = eq(endSentinel)

    def unlink = {
      require(this ne startSentinel, "can't unlink the start sentinel")
      require(this ne endSentinel, "can't unlink the end sentinel")
      next.prev = prev
      prev.next = next
      prev = null
      next = null
      count -= 1
      v
    }

    def follow(v: T) = {
      val node = new Node(this, next, v)

      next.prev = node
      next = node
      count += 1
      node
    }

    def precede(v: T) = {
      val node = new Node(prev, this, v)

      prev.next = node
      prev = node
      count += 1
      node
    }

    def iterator =
      new Iterator[Node] {
        private var node = Node.this

        def hasNext = node ne endSentinel

        def next = {
          if (isEmpty) throw new NoSuchElementException("no more elements")

          val res = node

          node = node.next
          res
        }
      }

    def reverseIterator =
      new Iterator[Node] {
        private var node = Node.this

        def hasNext = node ne startSentinel

        def next = {
          if (isEmpty) throw new NoSuchElementException("no more elements")

          val res = node

          node = node.prev
          res
        }
      }

    override def toString: String = s"node[$v]"
  }

  class Sentinel(name: String) extends Node {
    private def novalue = sys.error("sentinel has no value")

    override def element = novalue

    override def element_=(v: T) = novalue

    private def noiterator = sys.error("can't iterate from sentinel")

    override def iterator =
      if (this eq startSentinel)
        noiterator
      else
        super.iterator

    override def reverseIterator =
      if (this eq endSentinel)
        noiterator
      else
        super.reverseIterator

    override def toString: String = name
  }

  val startSentinel = new Sentinel("start sentinel")
  val endSentinel = new Sentinel("end sentinel")

  private var count = 0

  clear

  //
  // DLList operations
  //

  def appendElement(elem: T) = endSentinel precede elem

  def headNode = {
    require(nonEmpty, "list is empty")
    startSentinel.next
  }

  def lastNode = {
    require(nonEmpty, "list is empty")
    endSentinel.prev
  }

  def node(n: Int) = {
    require(0 <= n && n < count, s"node index out of range: $n")

    if (n == 0)
      headNode
    else if (n == count - 1)
      lastNode
    else if (n <= count / 2)
      nodeIterator drop n next
    else
      reverseNodeIterator drop (count - 1 - n) next
  }

  def nodeIterator = startSentinel.next.iterator

  def reverseNodeIterator = endSentinel.prev.reverseIterator

  def prependElement(elem: T) = startSentinel follow elem

  //
  // abstract Buffer methods
  //

  def addOne(elem: T) = {
    appendElement(elem)
    this
  }

  def prepend(elem: T) = {
    prependElement(elem)
    this
  }

  def apply(n: Int) = node(n).v

  def clear: Unit = {
    startSentinel.next = endSentinel
    endSentinel.prev = startSentinel
    count = 0
  }

  def iterator = nodeIterator map (_.v)

  def length = count

  def insertAll(n: Int, elems: IterableOnce[T]) = {
    var prev = node(n)

    elems.iterator foreach (e => prev = prev follow e)
  }

  def remove(n: Int) = node(n).unlink

  def update(n: Int, newelem: T): Unit = node(n).v = newelem

  //
  // overrides
  //

  override def head = headNode.v

  override def last = lastNode.v

  override def reverseIterator = reverseNodeIterator map (_.v)

  override def toString = iterator mkString ("DLList(", ", ", ")")

}
