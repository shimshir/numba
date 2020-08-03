package numba

class NumeralSet private (private val numerals: Seq[Numeral]) {
  def reverse = new NumeralSet(numerals.reverse)
  def without(chars: Char*): NumeralSet = {
    new NumeralSet(numerals.filterNot(n => chars.contains(n.value)))
  }
}

object NumeralSet {
  val Arabic: NumeralSet = NumeralSet("0123456789")
  val Alphabetic: NumeralSet = NumeralSet("ABCDEFGHIJKLMNOPQRSTUVWXYZ")

  def apply(symbols: String): NumeralSet =
    new NumeralSet(symbols.toCharArray.distinct.map(Numeral.apply))
  def numeralAtIndex(numeralSet: NumeralSet, int: Int): Numeral =
    numeralSet.numerals(int)
  def contains(numeralSet: NumeralSet, numeral: Numeral): Boolean =
    numeralSet.numerals.contains(numeral)
  def min(numeralSet: NumeralSet): Numeral = numeralSet.numerals.head
  def max(numeralSet: NumeralSet): Numeral = numeralSet.numerals.last
  def base(numeralSet: NumeralSet): Int = numeralSet.numerals.size
  def rank(numeralSet: NumeralSet, numeral: Numeral): Int = {
    if (contains(numeralSet, numeral)) {
      numeralSet.numerals.indexOf(numeral)
    } else {
      throw new IllegalArgumentException(
        s"numeral: $numeral is not part of numeral set: $numeralSet"
      )
    }
  }
  def next(numeralSet: NumeralSet, numeral: Numeral): Numeral = {
    val r = rank(numeralSet, numeral)
    if (r == base(numeralSet) - 1) {
      min(numeralSet)
    } else {
      numeralSet.numerals(r + 1)
    }
  }
}
