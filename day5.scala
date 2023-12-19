// https://adventofcode.com/2023/day/5

@main def run = {
  val input =
    """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""

  val inputLines = input.split("\n\n")

  val seeds = inputLines.head.split(' ').tail.map(_.toLong)
  val sections = inputLines.tail

  val allCategories = sections.map { section =>
    section.split('\n').tail.map(_.split(' ').map(_.toLong))
  }

  var lowestLocation: Option[Long] = None

  seeds.foreach(seed => {
    var currentSourceNumber = seed
    allCategories.zipWithIndex.foreach((categoryArrays, index) => {

      var destinationNumber = currentSourceNumber

      categoryArrays.foreach((array) => {
        val sourceRange = array(1) until array(1) + array(2)
        if (sourceRange.contains(currentSourceNumber)) {
          destinationNumber = array(0) + (currentSourceNumber - array(1))
        }
      })

      currentSourceNumber = destinationNumber

      if (index == allCategories.length - 1) {
        lowestLocation match {
          case None => lowestLocation = Some(currentSourceNumber)
          case Some(value) =>
            if (value > currentSourceNumber) {
              lowestLocation = Some(currentSourceNumber)
            }
        }
      }
    })
  })

  println(lowestLocation.get)
}

run
