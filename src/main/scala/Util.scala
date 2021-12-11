package io.andrewsmith.advent_of_code_2021

import scala.io.Source
import scala.reflect.ClassTag
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

  trait Grid[T] {
    val rows: Seq[Seq[T]]

    private val width = rows.head.size
    private val height = rows.size

    def apply(x: Int, y: Int): Option[T] = {
      if (0 <= x && x < width && 0 <= y && y < height) {
        Some(rows(y)(x))
      } else {
        None
      }
    }

    def values: Set[T] = {
      rows.flatten.toSet
    }

    def mapValues[B](func: T => B): Seq[Seq[B]] = {
      rows.map(row => row.map(func))
    }
  }

  def parseGridValues[T : ClassTag](lines: Seq[String], parser: (String, Int, Int) => T, cellSeparator: String = ""): Seq[Seq[T]] = {
    lines.zipWithIndex.map {
      case (line, y) => line.split(cellSeparator).zipWithIndex.map {
        case (cell, x) => parser(cell, x, y)
      }.toVector
    }.toVector
  }
}
