package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day1 {
  def numberOfDepthIncreases(fileName: String): Int = {
    numberOfDepthIncreases(readLines(fileName))
  }

  def numberOfDepthIncreases(lines: Seq[String]): Int = {
    lines.map(_.toInt).sliding(2).count {
      pair => pair(0) < pair(1)
    }
  }
}
