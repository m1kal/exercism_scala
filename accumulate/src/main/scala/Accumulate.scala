class Accumulate {
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = {
    list match {
      case Nil => List()
      case elem :: rest => f(elem) :: accumulate(f, rest)
    }
  }
}

