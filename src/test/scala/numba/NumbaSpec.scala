package numba

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class NumbaSpec extends AnyFlatSpec with Matchers {
  val AlAlArArAlAlArAr =
    Seq(
      NumeralSet.Alphabetic,
      NumeralSet.Alphabetic,
      NumeralSet.Arabic,
      NumeralSet.Arabic,
      NumeralSet.Alphabetic,
      NumeralSet.Alphabetic,
      NumeralSet.Arabic,
      NumeralSet.Arabic
    )

  "Numba" should "be convertible from and to Int" in {
    (1 to 10000).foreach { b10 =>
      val fromB10 = Numba.fromBase10(b10, AlAlArArAlAlArAr)
      val toB10 = Numba.toBase10(fromB10)
      //println((b10, fromB10, toB10))
      b10 shouldEqual toB10
    }
  }

  "Numba" should "be convertible from and to any Int" in {
    val r = new Random()
    (1 to 100).foreach { _ =>
      val b10 = BigInt(r.nextInt(Int.MaxValue))
      val fromB10 = Numba.fromBase10(b10, AlAlArArAlAlArAr)
      val toB10 = Numba.toBase10(fromB10)
      //println((b10, fromB10, toB10))
      b10 shouldEqual toB10
    }
  }

  "Numba" should "be random" in {
    Numba.random(AlAlArArAlAlArAr) should not equal
      Numba.random(AlAlArArAlAlArAr)
    Numba.random(AlAlArArAlAlArAr, 42).toString shouldEqual "AH84AR58"
  }
}
