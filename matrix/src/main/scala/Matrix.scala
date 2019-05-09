case class Matrix(input : String) {
  val row = input split("\n") map {
    r => r split(" ") map(_.toInt)
  }
  val column = (c : Int) => row.map(_(c))
}
