package sumito3478.collection.mutable

import scala.collection.generic.CanBuildFrom
import scala.collection.generic.MutableMapFactory
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.Map
import scala.collection.mutable.MapLike
import sumito3478.HashCoder
import sumito3478.collection.ThreadLocal
import sumito3478.Dummy0

trait UnorderedMap[@specialized(
  Byte, Short, Char, Long, Float, Double) A, @specialized(
  Byte, Short, Char, Long, Float, Double) B]
  extends Map[A, B]
  with MapLike[A, B, UnorderedMap[A, B]] {
  self =>

  protected[this] def seed = {
    val seeder = UnorderedMap.seeder()
    (seeder.nextLong, seeder.nextLong)
  }

  protected[this] var _seed = seed

  protected[this] val hashCoder: HashCoder[A]

  protected[this] val initialCapacity = 11

  private[this] var table = Array.tabulate(initialCapacity)(_ => List.empty[(A, B)])

  private[this] var tableLen = initialCapacity

  private[this] var _size = 0

  override def size: Int = _size

  override def foreach[U](f: ((A, B)) => U): Unit = {
    iterator foreach f
  }

  var loadFactor = 1 // threshold = tableLen / (2 ^ loadFactor)

  def threshold = {
    tableLen >>> loadFactor
  }

  private[this] def findIndex(k: A): Int = {
    val h = hashCoder.hashCode(k, _seed._1, _seed._2) & 0x7fffffffffffffffL
    (h % tableLen).toInt
  }

  private[this] def rehash(): this.type = {
    val tmp = toVector
    val newLen = (tableLen << 1) + 1
    val newTable = Array.tabulate(newLen)(_ => List.empty[(A, B)])
    table = newTable
    tableLen = newLen
    _seed = seed // reseed
    tmp foreach {
      e =>
        val idx = findIndex(e._1)
        table(idx) = e :: table(idx)
    }
    this
  }

  def +=(kv: (A, B)): this.type = {
    val idx = findIndex(kv._1)
    if ((table(idx) find (_._1 == kv._1)).isEmpty) {
      _size += 1
    }
    if (threshold < _size) {
      rehash
      val idx = findIndex(kv._1)
      table(idx) = kv :: (table(idx) filter (_._1 != kv._1))
    } else {
      table(idx) = kv :: (table(idx) filter (_._1 != kv._1))
    }
    this
  }

  def -=(k: A): this.type = {
    val idx = findIndex(k)
    if ((table(idx) find (_._1 == k)).nonEmpty) {
      _size -= 1
    }
    table(idx) = table(idx) filterNot (_._1 == k)
    this
  }

  def get(k: A): Option[B] = {
    val idx = findIndex(k)
    table(idx).find(_._1 == k).map(_._2)
  }

  override def toVector: Vector[(A, B)] = {
    val builder = new VectorBuilder[(A, B)]
    table foreach (builder ++= _)
    val ret = builder.result
    _size = ret.size
    ret
  }

  def iterator: Iterator[(A, B)] = {
    toVector.iterator
  }

  def empty[T >: UnorderedMap[A, B]](implicit dummy0: Dummy0): UnorderedMap[A, B]

  override def empty: UnorderedMap[A, B] = {
    empty[UnorderedMap[A, B]]
  }
}

object UnorderedMap extends MutableMapFactory[UnorderedMap] {
  private[this] class Concrete[A, B](val hashCoder: HashCoder[A]) extends AbstractMapLike[A, B, UnorderedMap[A, B]] with UnorderedMap[A, B] {
    def empty[T >: UnorderedMap[A, B]](implicit dummy0: Dummy0): UnorderedMap[A, B] = {
      UnorderedMap.empty[A, B](hashCoder)
    }
  }

  val seeder = ThreadLocal(() => new java.util.Random)

  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), UnorderedMap[A, B]] = new MapCanBuildFrom[A, B]

  def empty[A, B](implicit coder: HashCoder[A]): UnorderedMap[A, B] = new Concrete(coder)

  def empty[A, B] = empty(HashCoder.AnyHashCoder)
}