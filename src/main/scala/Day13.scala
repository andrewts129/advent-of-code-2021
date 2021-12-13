package io.andrewsmith.advent_of_code_2021

import Util.{readLines, repartition}

object Day13 {
  case class Point(x: Int, y: Int)

  object Point {
    def apply(line: String): Point = {
      val Array(x, y) = line.split(",")

      Point(x.toInt, y.toInt)
    }
  }

  case class Paper(points: Set[Point]) {
    def apply(instruction: Instruction): Paper = {
      instruction match {
        case HorizontalFold(crease) => apply(
          point => Point(point.x, if (point.y < crease) point.y else crease - (point.y - crease))
        )
        case VerticalFold(crease) => apply(
          point => Point(if (point.x < crease) point.x else crease - (point.x - crease), point.y)
        )
      }
    }

    private def apply(func: Point => Point): Paper = {
      Paper(points.map(func))
    }

    def numVisibleDots: Int = {
      points.size
    }
  }

  trait Instruction {
    val crease: Int
  }

  object Instruction {
    def apply(line: String): Instruction = {
      val Array(axis, crease) = line.replace("fold along ", "").split("=")

      axis match {
        case "x" => VerticalFold(crease.toInt)
        case "y" => HorizontalFold(crease.toInt)
      }
    }
  }

  case class VerticalFold(crease: Int) extends Instruction
  case class HorizontalFold(crease: Int) extends Instruction

  def dotsVisibleAfterFirstFold(fileName: String): Int = {
    val (paper, instructions) = parse(fileName)

    paper.apply(instructions.head).numVisibleDots
  }

  private def parse(fileName: String): (Paper, Seq[Instruction]) = {
    val Seq(pointLines, instructionLines) = repartition(readLines(fileName))

    (Paper(pointLines.map(Point(_)).toSet), instructionLines.map(Instruction(_)))
  }
}
