object ProteinTranslation {
  val Translations = Map("AUG"->"Methionine",
                         "UUU, UUC"->"Phenylalanine",
                         "UUA, UUG"->"Leucine",
                         "UCU, UCC, UCA, UCG"->"Serine",
                         "UAU, UAC"->"Tyrosine",
                         "UGU, UGC"-> "Cysteine",
                         "UGG"->"Tryptophan",
                         "UAA, UAG, UGA" -> "")
  def getProtein(input : String) = Translations(Translations.keys.find(x=>x contains input).get)
  def proteins(input : String) =  input.sliding(3,3).toList.map(getProtein).takeWhile(_.nonEmpty)
}
