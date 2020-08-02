object Allergen extends Enumeration {
  type Allergen = Value
  val Eggs, Peanuts, Shellfish, Strawberries,
      Tomatoes, Chocolate, Pollen, Cats = Value
}

object Allergies {
  val allergens = Allergen.values.toArray
  def allergicTo(allergy: Allergen.Allergen, score: Int) = {
    (score >> allergens.indexOf(allergy)) % 2 == 1
  }
  def list(score: Int) = {
    allergens.filter(allergicTo(_, score))
  }
}
