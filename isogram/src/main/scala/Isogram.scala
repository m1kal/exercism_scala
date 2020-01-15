object Isogram {
  def isIsogram(string : String) = isIsogram_(string.toLowerCase().replaceAll("[- ]",""))
  def isIsogram_(string : String) = string.toSet.size == string.size
}

