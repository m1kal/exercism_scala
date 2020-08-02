object PerfectNumbers {
  private def factorSum(value : Int) : Int = {
    (1 until value).filter(value % _ == 0).foldLeft(0)(_+_)
  }

  def classify(value : Int) : Either[String, NumberType.NumberType] = {
    if (value <= 0)
      Left("Classification is only possible for natural numbers.")
    else
      factorSum(value) compare value signum match {
        case 0 => Right(NumberType.Perfect)
        case 1 => Right(NumberType.Abundant)
        case -1 => Right(NumberType.Deficient)
      }
  }
}

