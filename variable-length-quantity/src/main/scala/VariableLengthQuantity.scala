object VariableLengthQuantity {

  def encode(input: List[Int]) : List[BigInt] = input.flatMap(encodeSingle)

  private def encodeSingle(n: Int) = {
    val byteRange = (0 until (n.toBinaryString.length / 7.0).ceil.toInt).toList
    val m = if (n < 0) 0x100000000L + BigInt(n) else BigInt(n)
    byteRange.map(k => {(m/(1<<7*k) % 128) + (if (k > 0) 128 else 0)}).reverse
  }

  private def split(input: List[Int], lists: List[List[Int]], current : List[Int]) :List[List[Int]]=  {
    input match {
      case Nil => lists
      case x :: y if x < 128 => split(y, lists :+ (current :+ x), List())
      case x :: y => split(y, lists, current :+ x)
    }
  }

  def decode(input: List[Int]) : Either[List[Int], List[Int]] = {
    if (input.last > 127)
      Left(List())
    else
      Right((split(input, List(), List()).map(decodeValue)))
  }

  private def decodeValue(input: List[Int]) = {
    input.map(x => {x % 128}).foldLeft(0)((acc,x) => (acc<<7) + x)
  }
}

