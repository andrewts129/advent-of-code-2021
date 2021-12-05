package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day5 {
  case class Point(x: Int, y: Int)

  object Point {
    def parse(string: String): Point = {
      val Array(x, y) = string.split(",")

      Point(x.toInt, y.toInt)
    }
  }

  case class Line(start: Point, end: Point) {
    def intersections(other: Line): Set[Point] = {
      pointsCovered.intersect(other.pointsCovered)
    }

    def isHorizontal: Boolean = {
      start.y == end.y
    }

    def isVertical: Boolean = {
      start.x == end.x
    }

    private def pointsCovered: Set[Point] = {
      if (isVertical) {
        range(start.y, end.y).map(Point(start.x, _)).toSet
      } else if (isHorizontal) {
        range(start.x, end.x).map(Point(_, start.y)).toSet
      } else {
        ???
      }
    }
  }

  object Line {
    def parse(string: String): Line = {
      val Array(start, end) = string.split(" -> ")

      Line(Point.parse(start), Point.parse(end))
    }
  }

  def numberOfOverlapPoints(fileName: String): Int = {
    val lines = readLines(fileName).map(Line.parse)
    numberOfOverlapPoints(
      lines.filter(line => line.isHorizontal || line.isVertical)
    )
  }

  private def numberOfOverlapPoints(lines: Seq[Line]): Int = {
    pairs(lines).flatMap {
      case (line, otherLine) => line.intersections(otherLine)
    }.size
  }

  private def pairs(lines: Seq[Line]): Set[(Line, Line)] = {
    lines.toSet.subsets(2).map { subset =>
      val vectorized = subset.toVector
      (vectorized(0), vectorized(1))
    }.toSet
  }

  private def range(a: Int, b: Int): Range.Inclusive = {
    if (a <= b) {
      a.to(b, 1)
    } else {
      a.to(b, -1)
    }
  }
}
