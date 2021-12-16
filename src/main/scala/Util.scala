package io.andrewsmith.advent_of_code_2021

import io.andrewsmith.advent_of_code_2021.Util.Grid.Point

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

    val width: Int = rows.head.size
    val height: Int = rows.size

    def apply(x: Int, y: Int): Option[T] = {
      if (inBounds(x, y)) {
        Some(rows(y)(x))
      } else {
        None
      }
    }

    def apply(point: Point): Option[T] = {
      this(point.x, point.y)
    }

    def inBounds(x: Int, y: Int): Boolean = {
      0 <= x && x < width && 0 <= y && y < height
    }

    def inBounds(point: Point): Boolean = {
      this.inBounds(point.x, point.y)
    }

    def values: Set[T] = {
      rows.flatten.toSet
    }

    def mapValues[B](func: T => B): Seq[Seq[B]] = {
      rows.map(row => row.map(func))
    }

    def allPoints: Set[Point] = {
      (0 until width).flatMap {
        x => (0 until height).map {
          y => Point(x, y)
        }
      }.toSet
    }
  }

  object Grid {
    case class Point(x: Int, y: Int) {
      def left: Point = {
        Point(x - 1, y)
      }

      def right: Point = {
        Point(x + 1, y)
      }

      def up: Point = {
        Point(x, y - 1)
      }

      def down: Point = {
        Point(x, y + 1)
      }

      def adjacent: Set[Point] = {
        Set(left, right, up, down)
      }
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
