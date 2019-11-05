case class Bowling( val rolls : List[Int] = List()) {
  def roll(value : Int) = Bowling(rolls :+ value)
  var done : Boolean = false
  def makeFrames(rolls:Seq[Int], frames:Array[Int] = Array()) : Array[Int] = {
    if (frames.length == 9) {
      if ((rolls(0) == 10) || (rolls.length > 1 && rolls(0)+rolls(1) == 10))
        done = rolls.length == 3;
      else
        done = rolls.length == 2;
    }
    rolls match {
    case 10 :: rest => makeFrames(rest, frames:+rest.take(2).foldLeft(10)(_+_))
    case a :: b :: c :: rest if a+b==10 => makeFrames(c::rest, frames:+(a+b+c))
    case a :: b :: rest if a+b > 10 => {done = false; Array(-1)}
    case a :: b :: rest => makeFrames(rest, frames:+(a+b))
    case _ => frames
    }
  }
  def invalid(rolls: Seq[Int], frames : Array[Int]) : Boolean = {
    rolls.find(_<0).nonEmpty || rolls.find(_>10).nonEmpty || !done
  }
  def score() :Either[String, Int]= {
    val frames = makeFrames(rolls)
    if (invalid(rolls, frames))
      Left("Invalid game!")
    else
      Right(frames.take(10).reduce(_+_))
  }
}
