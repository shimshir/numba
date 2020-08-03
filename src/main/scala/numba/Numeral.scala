package numba

class Numeral private (val value: Char) {
  override def toString: String = value.toString

  def canEqual(other: Any): Boolean = other.isInstanceOf[Numeral]

  override def equals(other: Any): Boolean =
    other match {
      case that: Numeral =>
        (that canEqual this) &&
          value == that.value
      case _ => false
    }

  override def hashCode(): Int = {
    val state = Seq(value)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Numeral {
  private[numba] def apply(value: Char): Numeral = {
    new Numeral(value)
  }
}
