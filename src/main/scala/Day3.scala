package io.andrewsmith.advent_of_code_2021

import Util.readLines

import scala.annotation.tailrec

object Day3 {
  def powerConsumption(fileName: String): Int = {
    powerConsumption(readLines(fileName))
  }

  def lifeSupportRating(fileName: String): Int = {
    lifeSupportRating(readLines(fileName))
  }

  private def powerConsumption(lines: Seq[String]): Int = {
    gammaRate(lines) * epsilonRate(lines)
  }

  private def gammaRate(numbers: Seq[String]): Int = {
    parseBinary(mostCommonCharacterAtEachPosition(numbers))
  }

  private def epsilonRate(numbers: Seq[String]): Int = {
    parseBinary(leastCommonCharacterAtEachPosition(numbers))
  }

  private def mostCommonCharacterAtEachPosition(strings: Seq[String]): String = {
    countCharactersAtEachPosition(strings).map {
      counts => counts.maxBy(_._2)._1
    }.mkString
  }

  private def leastCommonCharacterAtEachPosition(strings: Seq[String]): String = {
    countCharactersAtEachPosition(strings).map {
      counts => counts.minBy(_._2)._1
    }.mkString
  }

  private def countCharactersAtEachPosition(strings: Seq[String]): Array[Map[Char, Int]] = {
    strings.headOption match {
      case Some(head) => strings.foldLeft(
        Array.fill(head.length) { Map.empty[Char, Int] }
      ) {
        case (counts, string) => counts.zipWithIndex.map {
          case (countsAtIndex, index) => countsAtIndex.updated(string(index), countsAtIndex.getOrElse(string(index), 0) + 1)
        }
      }
      case None => Array()
    }
  }

  private def parseBinary(string: String): Int = Integer.parseInt(string, 2)

  private def lifeSupportRating(lines: Seq[String]): Int = {
    oxygenGeneratorRating(lines) * co2ScrubberRating(lines)
  }

  private def oxygenGeneratorRating(lines: Seq[String]): Int = {
    filterByBitCriteria(lines, mostCommonCharacterAtPosition)
  }

  private def co2ScrubberRating(lines: Seq[String]): Int = {
    filterByBitCriteria(lines, leastCommonCharacterAtPosition)
  }

  @tailrec
  def filterByBitCriteria(lines: Seq[String], bitCriteria: (Seq[String], Int) => Char, currentBitPosition: Int = 0): Int = {
    lines match {
      case Seq(value) => parseBinary(value)
      case _ =>
        val characterToFilterBy = bitCriteria(lines, currentBitPosition)

        filterByBitCriteria(
          lines.filter(_(currentBitPosition) == characterToFilterBy),
          bitCriteria,
          currentBitPosition + 1
        )
    }
  }

  private def mostCommonCharacterAtPosition(strings: Seq[String], position: Int): Char = {
    countCharactersAtEachPosition(strings)(position).maxBy {
      case (char, count) => (count, char)
    }._1
  }

  private def leastCommonCharacterAtPosition(strings: Seq[String], position: Int): Char = {
    countCharactersAtEachPosition(strings)(position).minBy {
      case (char, count) => (count, char)
    }._1
  }
}
