package sumito3478

import java.nio.ByteBuffer

class ByteBufferPointer(val intern: ByteBuffer, val pos: Int) extends Pointer {
  def getLong(): Long = {
    intern.getLong(pos)
  }
  
  def getInt(): Int = {
    intern.getInt(pos)
  }
  
  def get(): Byte = {
    intern.get(pos)
  }
  
  def getShort(): Short = {
    intern.getShort(pos)
  }
  
  def +(n: Int): Pointer = {
    new ByteBufferPointer(intern, pos + n)
  }
  
  def -(n: Int): Pointer = {
    this + (- n)
  }
}