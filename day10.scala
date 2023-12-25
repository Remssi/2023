// https://adventofcode.com/2023/day/10

@main def run = {
  val input =
    """..F7.
.FJ|.
SJ.L7
|F--J
LJ..."""

  enum Direction {
    case Up, Right, Down, Left
  }
  
  val grid = input.split("\n")

  val startPosition = {
    val x = grid.indexWhere(_.contains('S'))
    val y = grid(x).indexOf('S')
    (x, y)
  }

  var currentPosition = startPosition
  var previousPosition: (Int, Int) = startPosition

  var steps = 0

  def findPossibleNewPositions(): List[(Int, Int)] = {
  val gridRowLength = grid.length
  val gridColumnLength = if (gridRowLength > 0) grid(0).length else 0

  val currentPositionSymbol = grid(currentPosition._1)(currentPosition._2)
  val possibleDirections = currentPositionSymbol match {
    case '|' => Array(Direction.Up, Direction.Down)
    case '-' => Array(Direction.Left, Direction.Right)
    case '7' => Array(Direction.Left, Direction.Down)
    case 'J' => Array(Direction.Left, Direction.Up)
    case 'F' => Array(Direction.Right, Direction.Down)
    case 'L' => Array(Direction.Right, Direction.Up)
    case _ => Direction.values
  }

  val possiblePositions = for {
    direction <- possibleDirections
    newPosition = direction match {
      case Direction.Up => (currentPosition._1 - 1, currentPosition._2)
      case Direction.Right => (currentPosition._1, currentPosition._2 + 1)
      case Direction.Down => (currentPosition._1 + 1, currentPosition._2)
      case Direction.Left => (currentPosition._1, currentPosition._2 - 1)
    }
    if newPosition._1 >= 0 && newPosition._1 < gridRowLength &&
       newPosition._2 >= 0 && newPosition._2 < gridColumnLength
    nextPositionSymbol = grid(newPosition._1)(newPosition._2)
    if newPosition != previousPosition && 
      ((direction == Direction.Up && Array('|', '7', 'F', 'S').contains(nextPositionSymbol)) ||
       (direction == Direction.Right && Array('-', 'J', '7', 'S').contains(nextPositionSymbol)) ||
       (direction == Direction.Down && Array('|', 'L', 'J', 'S').contains(nextPositionSymbol)) ||
       (direction == Direction.Left && Array('-', 'L', 'F', 'S').contains(nextPositionSymbol)))
  } yield newPosition

  possiblePositions.toList
}

  while (grid(currentPosition._1)(currentPosition._2) != 'S' || steps == 0) {
    val possibleNewPositions = findPossibleNewPositions()
    steps += 1
    previousPosition = currentPosition;
    currentPosition = possibleNewPositions.head
  }

  println(steps / 2)
}

run