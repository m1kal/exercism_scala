object Sieve {
  def primes(limit: Int) = {
    if (limit <2)
      List()
    else {
      var array = 2 +=: scala.collection.mutable.ListBuffer.range(3, limit + 1, 2)
      for (i <- 3 to Math.sqrt(limit).toInt; j <- 2 to limit/i + 1 if array.contains(i)) array -= i*j
      array
    }
  }
}

