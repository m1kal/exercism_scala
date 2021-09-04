object ArmstrongNumbers {
  def isArmstrongNumber(value: Int) : Boolean = {
    val digits = value.toString.split("").map(_.toInt)
    val sum = digits.map(Math.pow(_, digits.length)).reduce(_ + _)
    value == sum
  }
}

