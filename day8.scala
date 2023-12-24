// https://adventofcode.com/2023/day/8

@main def run = {
  val input =
    """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"""

  val inputLines = input.split("\n\n")
  val turnCommands = inputLines.head
  val nodeStrings = inputLines.tail.mkString.split("\n")

  val nodeMap: Map[String, (String, String)] = nodeStrings.map { nodeString =>
    val Array(left, right) = nodeString.split(" = ")
    val key = left.trim
    val values = right.stripPrefix("(").stripSuffix(")").split(", ").map(_.trim)
    key -> (values(0), values(1))
  }.toMap

  var totalTurns = 0

  val turnCommandsLastIndex = turnCommands.length() - 1
  var turnCommandIndex = 0

  var currentNode = "AAA"

  var goalFound = false
  while (!goalFound) {

    if (currentNode == "ZZZ") {
      goalFound = true
    }
    else {
      val nodeData = nodeMap(currentNode)
      val nextNode = if (turnCommands(turnCommandIndex) == 'L') nodeData(0) else nodeData(1)
      
      totalTurns += 1
      currentNode = nextNode

      if (turnCommandIndex == turnCommandsLastIndex) {
        turnCommandIndex = 0
      }
      else {
        turnCommandIndex += 1
      }
    }
  }
  
  println(totalTurns)
}

run