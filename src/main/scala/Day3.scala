package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day3 {
  def powerConsumption(fileName: String): Int = {
    powerConsumption(readLines(fileName))
  }

  private def powerConsumption(lines: Seq[String]): Int = {
    gammaRate(lines) * epsilonRate(lines)
  }

  private def gammaRate(numbers: Seq[String]): Int = {
    Integer.parseInt(mostCommonCharacterAtEachPosition(numbers), 2)
  }

  private def epsilonRate(numbers: Seq[String]): Int = {
    Integer.parseInt(leastCommonCharacterAtEachPosition(numbers), 2)

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
}
