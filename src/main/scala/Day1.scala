package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day1 {
  def numberOfDepthIncreases(fileName: String): Int = {
    numberOfDepthIncreases(readLines(fileName))
  }

  def numberOfDepthIncreases(lines: Seq[String]): Int = {
    numIncreasingPairs(lines.map(_.toInt))
  }

  def numberOfDepthIncreasesSliding(fileName: String): Int = {
    numberOfDepthIncreasesSliding(readLines(fileName))
  }

  def numberOfDepthIncreasesSliding(lines: Seq[String]): Int = {
    numIncreasingPairs(lines.map(_.toInt).sliding(3).map(_.sum).toList)
  }

  private def numIncreasingPairs(pairs: Seq[Int]): Int = {
    pairs.sliding(2).count {
      pair => pair(0) < pair(1)
    }
  }
}
