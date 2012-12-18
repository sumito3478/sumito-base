package sumito3478

import sumito3478.math.CityHash

trait HashCodable {
  def hashCode[A <: Long](implicit dummy0: Dummy0): Long
  
  override def hashCode: Int = {
    import CityHash.{fmix, mur}
    val h = hashCode[Long]
    val low = (h >>> 32).toInt
    val high = (h & 0xffffffff).toInt
    fmix(mur(low, high))
  }
}