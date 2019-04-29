class Accumulate {
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = {
    var out : List[B] = List()
    for (elem <- list) {
      out = out :+ f(elem)
    }
   out
  }
}

