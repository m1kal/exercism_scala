object CollatzConjecture {
  def steps(init: Int, soFar: Int = 0) : Option[Int] = init match {
    case x if x < 1 => None
    case 1 => Some(soFar)
    case x if (x % 2 == 0) => steps(init / 2, soFar + 1)
    case _ => steps(init * 3 + 1, soFar + 1)
  }
}
