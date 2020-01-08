object Raindrops {
  val sounds = Map(3 -> "Pling", 5 -> "Plang", 7 -> "Plong")
  def convert(n: Int): String = {
    val output = sounds map {case (k,v)=> if (n % k == 0) v else ""} mkString("")
    if (output.isEmpty) n.toString else output
  }
}

