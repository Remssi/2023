// https://adventofcode.com/2023/day/1
@main def run = {
  val calVals =
    """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""

  val calValsLines = calVals.split("\n")

  var totalSum = 0;

  calValsLines.foreach(line => {
    val firstDigit = line.find(char => char.isDigit).get
    val secondDigit = line.findLast(char => char.isDigit).get

    val calVal = s"${firstDigit}${secondDigit}"

    totalSum += calVal.toInt
  })

  println(totalSum)
}

run
