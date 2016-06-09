package ch.wsl.fireindices

object ImplicitConversions {
    
    import scala.language.implicitConversions
    
	//implicit def intToString(x: Int) = x.toString
    implicit def int2double(x: Int): Double = x.toDouble
    implicit def long2double(x: Long): Double = x.toDouble
    implicit def double2int(x: Double): Int = x.toInt
    implicit def double2long(x: Double): Long = x.toLong
	
    implicit def list_int2double(x: List[Int]): List[Double] = x.map(_.toDouble)
    implicit def list_long2double(x: List[Long]): List[Double] = x.map(_.toDouble)
    implicit def list_double2int(x: List[Double]): List[Int] = x.map(_.toInt)
    implicit def list_double2long(x: List[Double]): List[Long] = x.map(_.toLong)
	
	
}
