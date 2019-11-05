object RomanNumerals {

  val digits = Array("I","V","X","L","C","D","M")

  def getLowestDigit(input : Int, idx : Int, mul : Int) : (Int, String)= {
    val remainder = input / mul
    if (input % mul == 4)
      (remainder & -2, digits(idx) + digits(idx + 1 + remainder % 2))
    else
      (remainder, digits(idx) * (input % mul))
  }

  def roman(input : Int, idx : Int =0, mul : Int =5, s : String ="") : String ={
    getLowestDigit(input, idx, mul) match {
      case (0, digit) => digit + s
      case (remainder, digit) => roman(remainder , idx + 1, mul ^ 7, digit + s)
    }
  }
}
