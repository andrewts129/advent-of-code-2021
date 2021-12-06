package io.andrewsmith.advent_of_code_2021

import Util.readLines

import scala.annotation.tailrec

object Day6 {
  case class Fish(numDaysUntilReproduction: Int) {
    def tomorrow: Seq[Fish] = {
      numDaysUntilReproduction match {
        case 0 => Seq(Fish(6), Fish(8))
        case _ => Seq(Fish(numDaysUntilReproduction - 1))
      }
    }
  }

  object Fish {
    def parseMany(string: String): Seq[Fish] = {
      string.split(",").map(Fish.parse)
    }

    def parse(string: String): Fish = {
      Fish(string.toInt)
    }
  }

  def numberOfFishAfter80Days(fileName: String): Long = {
    numberOfFishAfterNDays(readLines(fileName).flatMap(Fish.parseMany), 80)
  }

  @tailrec
  private def numberOfFishAfterNDays(fish: Seq[Fish], n: Int): Long = {
    if (n == 0) {
      fish.size
    } else {
      numberOfFishAfterNDays(fish.flatMap(_.tomorrow), n - 1)
    }
  }
}
