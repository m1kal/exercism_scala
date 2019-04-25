import scala.util.matching.Regex

object Bob {
  def response(statement: String): String = {
    val Question = "(.*\\?\\s*)".r
    val ForcefulQuestion = "([^a-z]*[A-Z][^a-z]*\\?\\s*)".r
    val Upcase = "([^a-z]*[A-Z][^a-z]*)".r
    val Silence = "(\\s*)".r
    statement match {
      case Silence(_) => "Fine. Be that way!"
      case ForcefulQuestion(_) => "Calm down, I know what I'm doing!"
      case Question(_) => "Sure."
      case Upcase(_) => "Whoa, chill out!"
      case _ => "Whatever."
    }
  }
}
