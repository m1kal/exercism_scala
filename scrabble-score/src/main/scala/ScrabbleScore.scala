object ScrabbleScore {
  val scores = Map("aeioulnrst"->1, "dg"->2,"bcmp"->3,"fhvwy"->4,"k"->5,"jx"->8,"qz"->10)
  def score(input : String) = {
    input.toLowerCase.map(x=>scores(scores.keys.find(_ contains x).get)).foldLeft(0)(_+_)
  }
}
