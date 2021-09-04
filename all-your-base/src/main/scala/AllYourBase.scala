object AllYourBase {
  def rebase(fromBase: Int, digits: List[Int], toBase: Int) = {
    if (invalidInput(fromBase, digits, toBase))
      None
    else
      Some(encode(digits.foldLeft(0)(_ * fromBase + _), toBase))
  }
  def encode(value: Int, base: Int, soFar: List[Int] = List()) : List[Int] = {
    if (value == 0)
      if (soFar.isEmpty) List(0) else soFar
    else
      encode(value / base, base, (value % base) :: soFar)
  }
  def invalidInput(fromBase: Int, digits: List[Int], toBase: Int) = {
    fromBase < 2 || toBase < 2 ||
      digits.exists(digit => digit < 0 || digit >= fromBase)
  }
}
