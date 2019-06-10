case class Matrix(input : String) {
  val row = input split("\n") map {
    r => r split(" ") map(_.toInt)
  }
  private val columns = collection.mutable.Map[Int,Array[Int]]()
  val column = (c : Int) => columns.getOrElseUpdate(c, row.map(_(c)))
}
