package io.andrewsmith.advent_of_code_2021

import Util.{Grid, parseGridValues, readLines}
import Util.Grid.Point

import scala.collection.mutable

object Day15 {

  case class Cavern(rows: Seq[Seq[Int]], expand: Boolean) extends Grid[Int] {
    private val trueWidth = rows.head.size
    private val trueHeight = rows.size

    override val width: Int = if (expand) trueWidth * 5 else trueWidth
    override val height: Int = if (expand) trueHeight * 5 else trueHeight

    private val end = Point(width - 1, height - 1)

    def lowestPathRisk: Int = {
      distances.apply(end)
    }

    override def apply(x: Int, y: Int): Option[Int] = {
      if (expand) {
        if (inBounds(x, y)) {
          Some(
            ((rows(y % trueHeight)(x % trueWidth) + (x / trueWidth) + (y / trueHeight) - 1) % 9) + 1
          )
        } else {
          None
        }
      } else {
        super.apply(x, y)
      }
    }

    private def distances: Map[Point, Int] = {
      val initialDistances = allPoints.toList.map {
        case start @ Point(0, 0) => (0, start)
        case other => (9999999, other)
      }

      distances(initialDistances)
    }

    private def distances(intermediate: List[(Int, Point)]): Map[Point, Int] = {
      val heap = mutable.PriorityQueue[(Int, Point)]()(
        Ordering.by(tup => (tup._1 + distanceFromEnd(tup._2)) * -1)
      )

      heap.addAll(intermediate)

      val result = mutable.Map[Point, Int]()

      while (!result.contains(end)) {
        val (cost, point) = heap.dequeue()

        if (!result.contains(point)) {
          result += (point -> cost)

          val neighbors = point.adjacent.flatMap {
            adjacentPoint => this (adjacentPoint) match {
              case Some(marginalCost) => Some((cost + marginalCost, adjacentPoint))
              case None => None
            }
          }

          heap.addAll(neighbors)
        }
      }

      result.toMap
    }

    private def distanceFromEnd(point: Point): Int = {
      math.sqrt(math.pow(end.x - point.x, 2) + math.pow(end.y - point.y, 2)).toInt
    }
  }

  object Cavern {
    def parse(lines: Seq[String], expand: Boolean = false): Cavern = {
      Cavern(
        parseGridValues(lines, (cell, _, _) => cell.toInt),
        expand
      )
    }
  }

  def lowestPathRisk(fileName: String): Int = {
    Cavern.parse(readLines(fileName)).lowestPathRisk
  }

  def lowestPathRiskExpanded(fileName: String): Int = {
    Cavern.parse(readLines(fileName), expand = true).lowestPathRisk
  }
}
