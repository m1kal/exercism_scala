object Say {
  val Digits = List("zero", "one", "two", "three", "four", "five",
                    "six", "seven", "eight", "nine",
                    "ten", "eleven", "twelve", "thirteen", "fourteen",
                    "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
  val Tens = List("","","twenty", "thirty", "forty", "fifty",
                        "sixty", "seventy", "eighty", "ninety")

  def format(thousands: String, rest: String, separator:String = "thousand") = {
    (thousands, rest) match {
      case ("zero", rest) => rest
      case (thousands, "zero") => thousands + " " + separator
      case (thousands, rest) => thousands + " " + separator + " " + rest
    }
  }

  def formatXX(tens: String, ones: String) = {
    (tens, ones) match {
      case ("", ones) => ones
      case (tens, "zero") => tens
      case (tens, ones) => tens+"-"+ones
    }
  }

  def inEnglish(value : Long) = {
    if ((1000000000000l > value) && (value >= 0)) Some(say(value)) else None
  }

  def say(value : Long) : String = {
    format(formatThousand(value/1000000000),
           format(formatThousand(value/1000000),
                  format(formatThousand(value/1000),
                         formatThousand(value%1000)),
                  "million"),
           "billion")
  }
  def formatThousand(value : Long) : String = {
      format(
        Digits((value.toInt / 100) % 10),
        formatXX(
          Tens((value.toInt / 10) % 10),
          Digits(
            if (value.toInt>20) (value.toInt % 10) else (value.toInt % 20))),
        "hundred"
        )
  }
}
