// https://adventofcode.com/2023/day/1

val calibrationDocument = Array(/* insert puzzle input here */)

var totalCalibrationValue = 0;

/*
 get the line's calibration value (first + last digit) and add to total
 could be improved with breaking as soon as first and last digits are found
*/
for (calibrationDocumentLine <- calibrationDocument) {
  val digitPattern = "\\d".r
  val allMatches = digitPattern.findAllIn(calibrationDocumentLine).toList

  allMatches match {
    case Nil    => println("No digit found in the string")
    case digits =>
      // get the first and last digits (can be same)
      val firstDigit = digits.head
      val lastDigit = digits.last

      // concat the first and last digits
      totalCalibrationValue += (firstDigit + lastDigit).toInt

  }
}

println(totalCalibrationValue)
