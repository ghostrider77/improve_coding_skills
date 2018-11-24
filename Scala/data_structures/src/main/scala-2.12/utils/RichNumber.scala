package utils


sealed trait RichNumber[T] {
  def mod(modulus: T): T
}

object RichNumber {
  implicit class RichInt(n: Int) extends RichNumber[Int] {
    def mod(modulus: Int): Int = {
      val remainder: Int = n % modulus
      if (remainder < 0) remainder + modulus else remainder
    }
  }

  implicit class RichLong(n: Long) extends RichNumber[Long] {
    def mod(modulus: Long): Long = {
      val remainder: Long = n % modulus
      if (remainder < 0) remainder + modulus else remainder
    }
  }
}
