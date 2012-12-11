package sumito3478.math

trait CRC32C extends CRC32{
  val poly: Int = 0x1edc6f41
}

object CRC32C extends CRC32C{

}