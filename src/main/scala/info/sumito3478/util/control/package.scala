package info.sumito3478.util

import System.nanoTime

package object control {
  /**
   * Dummy class to make scaladoc generate documentations for the pacakge
   * object.
   */
  @Deprecated
  class Dummy

  def breakable(op: (=> Nothing) => Unit): Unit = {
    op({ return })
  }

  /**
   * Executes op 1 time and returns the execution time in micro sedonds.
   */
  def time(op: () => Unit): Long = {
    val start = nanoTime
    op()
    (nanoTime - start) / 1000
  }

  /**
   * Executes op (n * 10000) times and returns the execution time in micro
   * seconds.
   *
   * @note This method executes op 10000 times before benchmarking to warm up.
   */
  def benchmark(n: Int, op: () => Unit): Long = {
    0 to 10000 foreach {
      i =>
        op()
    }
    var i = 0
    val limit = n * 10000
    val start = nanoTime
    while (i < limit) {
      op()
      i += 1
    }
    (nanoTime - start) / 1000
  }
}