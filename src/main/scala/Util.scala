package io.andrewsmith.advent_of_code_2021

import scala.io.Source
import scala.util.Using

object Util {
  def readLines(fileName: String): Seq[String] = {
    Using(Source.fromFile(fileName))(_.getLines().toList).get
  }

  def repartition(lines: Seq[String]): Seq[Seq[String]] = {
    if (lines.isEmpty) {
      Seq()
    } else {
      val (firstGroup, rest) = lines.span(_.nonEmpty)
      Seq(firstGroup) ++ repartition(rest.drop(1))
    }
  }
}
