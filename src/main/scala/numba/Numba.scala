package numba

import scala.annotation.tailrec
import scala.util.Random

class Numba private (
    val value: Seq[Numeral],
    val numeralSets: Seq[NumeralSet]
) {
  override def toString: String = value.map(_.toString).mkString("")

  def canEqual(other: Any): Boolean = other.isInstanceOf[Numba]

  override def equals(other: Any): Boolean =
    other match {
      case that: Numba =>
        (that canEqual this) &&
          value == that.value &&
          numeralSets == that.numeralSets
      case _ => false
    }

  override def hashCode(): Int = {
    val state = Seq(value, numeralSets)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Numba {
  def min(numeralSets: Seq[NumeralSet]): Numba = {
    new Numba(
      numeralSets.map(NumeralSet.min),
      numeralSets
    )
  }
  def max(numeralSets: Seq[NumeralSet]): Numba = {
    new Numba(
      numeralSets.map(NumeralSet.max),
      numeralSets
    )
  }
  def random(numeralSets: Seq[NumeralSet]): Numba = random(numeralSets, None)
  def random(numeralSets: Seq[NumeralSet], seed: Int): Numba =
    random(numeralSets, Some(seed))
  private def random(numeralSets: Seq[NumeralSet], seed: Option[Int]): Numba = {
    val r = seed.map(new Random(_)).getOrElse(new Random())
    new Numba(
      numeralSets.map(ns =>
        NumeralSet.numeralAtIndex(ns, r.nextInt(NumeralSet.base(ns)))
      ),
      numeralSets
    )
  }
  def inc(number: Numba): Numba = {
    new Numba(inc(number.value, Nil, number.numeralSets), number.numeralSets)
  }

  @tailrec
  private def inc(
      remaining: Seq[Numeral],
      processed: Seq[Numeral],
      numeralSets: Seq[NumeralSet]
  ): Seq[Numeral] = {
    if (remaining.isEmpty) {
      numeralSets.map(NumeralSet.min)
    } else {
      val leastSignificant: Numeral = remaining.last
      val numeralSet = numeralSets(remaining.size - 1)
      val nextNumeral = NumeralSet.next(numeralSet, leastSignificant)
      if (leastSignificant == NumeralSet.max(numeralSet)) {
        inc(
          remaining.init,
          nextNumeral +: processed,
          numeralSets
        )
      } else {
        (remaining.init :+ nextNumeral) ++ processed
      }
    }
  }

  def fromBase10(number: BigInt, numeralSets: Seq[NumeralSet]): Numba = {
    val maxBase10NumberPossible =
      numeralSets.map(ns => BigInt(NumeralSet.base(ns))).product
    if (maxBase10NumberPossible < number) {
      throw new IllegalArgumentException(
        s"base10 number: $number is too large for given numeral sets, maximal number is: $maxBase10NumberPossible"
      )
    } else {
      fromBase10(number, numeralSets, 0, Nil)
    }
  }

  @tailrec
  private def fromBase10(
      number: BigInt,
      numeralSets: Seq[NumeralSet],
      significance: Int = 0,
      result: Seq[Numeral] = Nil
  ): Numba = {
    if (significance > numeralSets.size - 1) {
      new Numba(result, numeralSets)
    } else {
      val numeralSet = numeralSets((numeralSets.size - 1) - significance)
      val base = NumeralSet.base(numeralSet)
      val quotient = number / base
      val remainder = (number % base).toInt
      fromBase10(
        quotient,
        numeralSets,
        significance + 1,
        NumeralSet.numeralAtIndex(numeralSet, remainder) +: result
      )
    }
  }

  def toBase10(numba: Numba): BigInt = {
    numba.value.zipWithIndex.map {
      case (numeral, index) =>
        val numeralSet = numba.numeralSets(index)
        val magnitude =
          ((index + 1) until numba.value.size)
            .map(idx => BigInt(NumeralSet.base(numba.numeralSets(idx))))
            .product
        BigInt(NumeralSet.rank(numeralSet, numeral)) * magnitude
    }.sum
  }
}
