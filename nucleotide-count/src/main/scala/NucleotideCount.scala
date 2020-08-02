class DNA(strand: String) {
  val Keys = "ACGT"
  def nucleotideCounts = {
    if (strand.filter(!Keys.contains(_)).nonEmpty)
      Left("Invalid nucleotides encountered.")
    else
      Right(Map() ++ Keys.map(x=>(x->strand.filter(_==x).length)))
  }
}
