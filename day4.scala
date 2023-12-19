// https://adventofcode.com/2023/day/4

@main def run = {
  val input = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""

  val inputLines = input.split("\n")

  var totalPoints = 0;

  inputLines.map(line => {
    val numbers = line.split(":")(1)

    def splitNumbers(index: Int): Array[Int] = {
      numbers
        .split('|')(index)
        .split(' ')
        .filter(_.nonEmpty)
        .map(_.toInt)
    }

    val winningNumbers = splitNumbers(0)

    val myNumbers = splitNumbers(1)

    var matchingNumbersCount = 0

    myNumbers.foreach(myNumber => {
      if (winningNumbers.contains(myNumber)) {
        matchingNumbersCount += 1
      }
    })

    matchingNumbersCount match {
      case 0 => {}
      case 1 => { totalPoints += 1 }
      case _ => totalPoints += math.pow(2, matchingNumbersCount - 1).toInt
    }
  })

  println(totalPoints)

}

run
