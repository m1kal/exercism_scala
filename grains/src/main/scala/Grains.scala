object Grains {
  def square(position: Int): Option[BigInt] = {
    if (0 < position && position < 65)
      Some(BigInt(1L << (position - 1)).abs)
    else
      None
  }
  def total = (BigInt(1) << 64) - BigInt(1)
}
