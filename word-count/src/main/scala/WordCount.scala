import scala.util.matching.Regex
case class WordCount(text: String) {
  val WordRegex = """(\w|(?<=\w)\'(?=\w))+""".r
  def countWords = {
    val words = WordRegex.findAllIn(text.trim.toLowerCase).toList
    words groupBy(identity) map{case (word, occ) => (word, occ.length)}
 }
}
