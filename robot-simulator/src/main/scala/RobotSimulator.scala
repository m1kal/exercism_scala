trait Bearing {
  val left:Bearing
  val right:Bearing
  val dx : Int
  val dy : Int
  def advance (position : Tuple2[Int,Int]) = {
    position match {
      case (x, y) => (x + dx, y + dy )
    }
  }
}

object Bearing {
  object South extends Bearing {
    override val (right, left, dx, dy) = (West, East, 0, -1)
  }
  object East extends Bearing {
    override val (right, left, dx, dy) = (South, North, 1, 0)
  }
  object North extends Bearing {
    override val (right, left, dx, dy) = (East, West, 0, 1)
  }
  object West extends Bearing {
    override val (right, left, dx, dy) = (North, South, -1, 0)
  }
}

case class Robot(bearing : Bearing, coordinates: Tuple2[Int, Int]) {
  private def execute(command: Char) = {
    command match {
      case 'A' => advance
      case 'L' => turnLeft
      case 'R' => turnRight
    }
  }
  def simulate(commands: String) : Robot = {
    commands.foldLeft(this) {(robot, command) => robot.execute(command)}
  }
  def advance = Robot(bearing, bearing.advance(coordinates))
  def turnRight = Robot(bearing.right,coordinates)
  def turnLeft = Robot(bearing.left,coordinates)
}

