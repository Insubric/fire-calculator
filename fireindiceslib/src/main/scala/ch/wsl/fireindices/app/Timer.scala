package ch.wsl.fireindices.app

import com.typesafe.scalalogging.LazyLogging

case class Timer() extends LazyLogging {
  private var t0:Long = System.nanoTime()
  def start() = t0 = System.nanoTime()
  def time(msg: String = "") = {
    val t1 = System.nanoTime()
    val timing = (t1 - t0) / 1000000000.0
    logger.warn(s"Elapsed time: ${(t1 - t0) / 1000000000.0}s $msg")
  }
}


object Timer extends LazyLogging {

  def mesure[R](msg: String = "")(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    logger.warn(s"Elapsed time: ${(t1 - t0) / 1000000000.0}s $msg")

    result
  }
}