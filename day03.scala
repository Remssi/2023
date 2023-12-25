// https://adventofcode.com/2023/day/3

import scala.collection.mutable.ListBuffer

@main def run = {
  val input =
    """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""
  val inputLines = input.split("\n")

  var totalSum = 0;

  var currentLineIndex = 0

  val lastLineIndex = inputLines.length - 1

  def charIsSymbolOtherThanDot(char: Char): Boolean = {
    char != '.' && !char.isLetterOrDigit
  }

  inputLines.foreach(line => {
    val lastCharIndex = line.length() - 1

    var currentNumberString = ""
    var currentNumberStartIndex = 0

    line.zipWithIndex.foreach((char, index) => {
      if (char.isDigit) {
        if (currentNumberString == "") {
          // store start of number
          currentNumberStartIndex = index
        }
        currentNumberString += char
      }
      if (!char.isDigit || index == lastCharIndex) {
        if (currentNumberString != "") {
          // when we get the whole number, check its arounds for a symbol
          val charIndexes = List.range(currentNumberStartIndex, index)

          var symbolFound = false
          for (charIndex <- charIndexes if !symbolFound) {
            var neighboursToCheck = ListBuffer[(Int, Int)]()

            for (i <- -1 to 1) {
              for (j <- -1 to 1) {
                if (!(i == currentLineIndex && j == charIndex)) {
                  val x = currentLineIndex + i
                  val y = charIndex + j

                  if (
                    x >= 0 && x <= lastLineIndex && y >= 0 && y <= lastCharIndex
                  ) {
                    neighboursToCheck += ((x, y))
                  }
                }
              }
            }

            if (
              neighboursToCheck.exists((x, y) =>
                charIsSymbolOtherThanDot(
                  inputLines(x)(y)
                )
              )
            ) {
              symbolFound = true;
            }
          }

          if (symbolFound) {
            totalSum += currentNumberString.toInt
          }

        }
        currentNumberString = ""
        currentNumberStartIndex = 0
      }
    })

    currentLineIndex += 1
  })

  println(totalSum)
}

run
