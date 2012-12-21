package info.sumito3478.text.collection.mutable

import scala.collection.mutable.Builder
import info.sumito3478.text.collection.immutable.StringText

import scala.collection.mutable.{StringBuilder => SStringBuilder}

class StringBuilder extends Builder[Char, StringText]{
  private[this] val intern = new SStringBuilder
  
  def +=(elem: Char): this.type = {
    intern += elem
    this
  }
  
  def clear(): Unit = {
    intern.clear()
  }
  
  def result(): StringText = {
    new StringText(intern.result)
  }
}