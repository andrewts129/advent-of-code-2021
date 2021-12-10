package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day9 {
  case class Location(x: Int, y: Int, height: Int) {
    def riskLevel: Int = {
      1 + height
    }
  }

  case class HeightMap(rows: Seq[Seq[Location]]) {
    private val width = rows.head.size
    private val height = rows.size

    def apply(x: Int, y: Int): Option[Location] = {
      if (0 <= x && x < width && 0 <= y && y < height) {
        Some(rows(y)(x))
      } else {
        None
      }
    }

    def sumOfRiskOfLowPoints: Int = {
      lowPoints.toVector.map(_.riskLevel).sum
    }

    private def points: Set[Location] = {
      rows.flatten.toSet
    }

    private def lowPoints: Set[Location] = {
      points.filter(point => isLowPoint(point.x, point.y))
    }

    private def isLowPoint(x: Int, y: Int): Boolean = {
      val point = this(x, y).get
      val neighbors = Set(this(x - 1, y), this(x + 1, y), this(x, y - 1), this(x, y + 1)).flatten

      neighbors.forall(_.height > point.height)
    }
  }

  object HeightMap {
    def parse(lines: Seq[String]): HeightMap = {
      val points = lines.zipWithIndex.map {
        case (line, y) => line.toCharArray.zipWithIndex.map {
          case (height, x) => Location(x, y, height.asDigit)
        }.toVector
      }

      HeightMap(points)
    }
  }

  def sumOfRiskOfLowPoints(fileName: String): Int = {
    HeightMap.parse(readLines(fileName)).sumOfRiskOfLowPoints
  }
}
