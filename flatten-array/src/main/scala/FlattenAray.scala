object FlattenArray {
  def flatten(input : List[Any]) : List[Any] = input match {
    case (x : List[Any]) :: xs => flatten(x) ::: flatten(xs)
    case null :: xs => flatten(xs)
    case x :: xs => x :: flatten(xs)
    case x => x
  }
}
