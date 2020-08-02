object HighScores {
  def latest(scores: List[Int]) = scores.last
  def personalTop(scores: List[Int]) = scores.sortWith(_>_).take(3)
  def personalBest(scores: List[Int]) = personalTop(scores).head
  def shortOf(value: Int) = if (value > 0) value + " short of " else ""
  def report(scores: List[Int]) = {
    val last = latest(scores)
    val short = personalBest(scores) - last
    s"Your latest score was $last. That's ${shortOf(short)}your personal best!"
  }
}
