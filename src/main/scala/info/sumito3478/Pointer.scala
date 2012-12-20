package info.sumito3478

import java.nio.ByteBuffer

trait Pointer {
  def getLong(): Long
  
  def getInt(): Int
  
  def get(): Byte
  
  def getShort(): Short
  
  def +(n: Int): Pointer
  
  def -(n: Int): Pointer
}
