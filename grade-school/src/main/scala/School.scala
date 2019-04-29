class School {
  type DB = Map[Int, Seq[String]]

  private var _db : DB = Map()

  def add(name: String, g: Int) = _db += (g -> (grade(g) :+ name))

  def db: DB = _db

  def grade(g: Int): Seq[String] = db.getOrElse(g,Seq[String]())

  def sorted: DB = db.keys.toList.sorted.map((k)=>k->db(k).sorted).toMap
}
