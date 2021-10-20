package io.github.edadma.commonmark

import scala.collection.mutable
import scala.language.postfixOps

object DLList {

  def apply[T](elems: T*): DLList[T] =
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

    def isBeforeStart: Boolean = eq(startSentinel)

    def isAfterEnd: Boolean = eq(endSentinel)

    def unlink: T = {
      require(this ne startSentinel, "can't unlink the start sentinel")
      require(this ne endSentinel, "can't unlink the end sentinel")
      next.prev = prev
      prev.next = next
      prev = null
      next = null
      count -= 1
      v
    }

    def follow(v: T): Node = {
      val node = new Node(this, next, v)

      next.prev = node
      next = node
      count += 1
      node
    }

    def precede(v: T): Node = {
      val node = new Node(prev, this, v)

      prev.next = node
      prev = node
      count += 1
      node
    }

    def iterator: Iterator[Node] =
      new Iterator[Node] {
        private var node = Node.this

        def hasNext: Boolean = node ne endSentinel

        def next(): Node = {
          if (isEmpty) throw new NoSuchElementException("no more elements")

          val res = node

          node = node.next
          res
        }
      }

    def reverseIterator: Iterator[Node] =
      new Iterator[Node] {
        private var node = Node.this

        def hasNext: Boolean = node ne startSentinel

        def next(): Node = {
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

    override def element: T = novalue

    override def element_=(v: T): Unit = novalue

    private def noiterator = sys.error("can't iterate from sentinel")

    override def iterator: Iterator[Node] =
      if (this eq startSentinel)
        noiterator
      else
        super.iterator

    override def reverseIterator: Iterator[Node] =
      if (this eq endSentinel)
        noiterator
      else
        super.reverseIterator

    override def toString: String = name
  }

  val startSentinel = new Sentinel("start sentinel")
  val endSentinel = new Sentinel("end sentinel")

  private var count = 0

  clear()

  //
  // DLList operations
  //

  def appendElement(elem: T): Node = endSentinel precede elem

  def headNode: Node = {
    require(nonEmpty, "list is empty")
    startSentinel.next
  }

  def lastNode: Node = {
    require(nonEmpty, "list is empty")
    endSentinel.prev
  }

  def node(n: Int): Node = {
    require(0 <= n && n < count, s"node index out of range: $n")

    if (n == 0)
      headNode
    else if (n == count - 1)
      lastNode
    else if (n <= count / 2)
      (nodeIterator drop n).next()
    else
      (reverseNodeIterator drop (count - 1 - n)).next()
  }

  def nodeIterator: Iterator[Node] = startSentinel.next.iterator

  def reverseNodeIterator: Iterator[Node] = endSentinel.prev.reverseIterator

  def prependElement(elem: T): Node = startSentinel follow elem

  //
  // abstract Buffer methods
  //

  def addOne(elem: T): DLList.this.type = {
    appendElement(elem)
    this
  }

  def prepend(elem: T): DLList.this.type = {
    prependElement(elem)
    this
  }

  def apply(n: Int): T = node(n).v

  def clear(): Unit = {
    startSentinel.next = endSentinel
    endSentinel.prev = startSentinel
    count = 0
  }

  def iterator: Iterator[T] = nodeIterator map (_.v)

  def length: Int = count

  def insert(n: Int, elem: T): Unit = {
    var prev = node(n)

    prev follow elem
  }

  def insertAll(n: Int, elems: IterableOnce[T]): Unit = {
    var prev = node(n)

    elems.iterator foreach (e => prev = prev follow e)
  }

  def remove(n: Int): T = node(n).unlink

  def update(n: Int, newelem: T): Unit = node(n).v = newelem

  def remove(idx: Int, count: Int): Unit = ???

  def patchInPlace(from: Int, patch: IterableOnce[T], replaced: Int): DLList.this.type = ???

  //
  // overrides
  //

  override def head: T = headNode.v

  override def last: T = lastNode.v

  override def reverseIterator: Iterator[T] = reverseNodeIterator map (_.v)

  override def toString: String = iterator mkString ("DLList(", ", ", ")")

}
