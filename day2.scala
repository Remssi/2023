// https://adventofcode.com/2023/day/2

import scala.util.matching.Regex.MatchIterator

@main def run = {
  val gameRoundsInput =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

  val redCubeLimit = 12;
  val greenCubeLimit = 13;
  val blueCubeLimit = 14;

  val gameRounds = gameRoundsInput.split("\n");

  var totalIdSum = 0;

  def getCubesMaxOfColor(color: String, gameRound: String): Int =
    (("""\d+ """ + color).r findAllIn gameRound)
      .map(splitCount)
      .toList
      .max

  /* get number from "0 color" format */
  def splitCount(cubeCounts: String): Int =
    cubeCounts.split(" ").head.toInt

  gameRounds.foreach((gameRound => {
    val gameId = ("""\d+""".r findFirstIn gameRound).get.toInt
    val redCubesMax = getCubesMaxOfColor("red", gameRound)
    val greenCubesMax = getCubesMaxOfColor("green", gameRound)
    val blueCubesMax = getCubesMaxOfColor("blue", gameRound)

    if (
      redCubesMax <= redCubeLimit && greenCubesMax <= greenCubeLimit && blueCubesMax <= blueCubeLimit
    ) {
      totalIdSum += gameId
    }
  }))

  println(totalIdSum)
}

run
