// https://adventofcode.com/2023/day/6

@main def run = {
  val input =
    """Time:      7  15   30
Distance:  9  40  200"""

  val inputLines = input.split("\n")

  def findIntegers(string: String): List[Int] = {
    ("""\d+""".r findAllIn string
      .split(":")
      .tail(0)).toList
      .map(_.toInt)
  }

  val raceTimes = findIntegers(inputLines.head)
  val recordDistances = findIntegers(inputLines.tail(0))

  var totalWinningStrategies = 1

  raceTimes.zipWithIndex.foreach((raceTime, raceIndex) => {
    val raceRecordDistance = recordDistances(raceIndex)

    var winningStrategiesInThisRace = 0
    // skip 0 and full race hold time
    for (holdTime <- 1 until raceTime) {
      if (holdTime * (raceTime - holdTime) > raceRecordDistance) {
        winningStrategiesInThisRace += 1
      }
    }

    if (winningStrategiesInThisRace > 0) {
      totalWinningStrategies *= winningStrategiesInThisRace
    }
  })

  println(totalWinningStrategies)
}

run
