object Darts {
  def score(x: Double, y: Double) = {
    x*x + y*y match {
      case dist if dist <= 1 => 10
      case dist if dist <= 25 => 5
      case dist if dist <= 100 => 1
      case _ => 0
    }
  }
}
