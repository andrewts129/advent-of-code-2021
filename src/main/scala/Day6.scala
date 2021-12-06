package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day6 {
  case class School(fishCounts: Map[Int, Long]) {
    def fishAfterNDays(n: Int): Long = {
      if (n == 0) {
        fishCounts.values.sum
      } else {
        tomorrow.fishAfterNDays(n - 1)
      }
    }

    def tomorrow: School = {
      // The compiler is being very strange here
      val huh: Long = fishCounts.getOrElse(7, 0)
      val what: Long = fishCounts.getOrElse(0, 0)
      val hmm: Long = huh + what
      val newCounts: Map[Int, Long] = Map(
        0 -> fishCounts.getOrElse(1, 0),
        1 -> fishCounts.getOrElse(2, 0),
        2 -> fishCounts.getOrElse(3, 0),
        3 -> fishCounts.getOrElse(4, 0),
        4 -> fishCounts.getOrElse(5, 0),
        5 -> fishCounts.getOrElse(6, 0),
        6 -> hmm,
        7 -> fishCounts.getOrElse(8, 0),
        8 -> what
      )

      School(newCounts)
    }
  }

  object School {
    def parse(lines: Seq[String]): School = {
      val startingFish = lines.flatMap(_.split(",")).map(_.toInt)

      School(
        startingFish.groupBy(identity).map {
          case (daysTilReproduction, fish) => daysTilReproduction -> fish.size
        }
      )
    }
  }

  def numberOfFishAfter80Days(fileName: String): Long = {
    School.parse(readLines(fileName)).fishAfterNDays(80)
  }

  def numberOfFishAfter256Days(fileName: String): Long = {
    School.parse(readLines(fileName)).fishAfterNDays(256)
  }
}
