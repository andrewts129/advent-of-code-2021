package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day2 {
  case class Delta(x: Int, y: Int) {
    def +(other: Delta): Delta = {
      Delta(x + other.x, y + other.y)
    }
  }

  object Delta {
    def parse(line: String): Delta = {
      line.split(" ", 2) match {
        case Array("forward", x) => Delta(x.toInt, 0)
        case Array("down", y) => Delta(0, y.toInt)
        case Array("up", y) => Delta(0, y.toInt * -1)
      }
    }
  }

  def productOfFinalPosition(fileName: String): Int = {
    productOfFinalPosition(readLines(fileName))
  }

  def productOfFinalPosition(lines: Seq[String]): Int = {
    val finalPosition = getFinalPosition(lines.map(Delta.parse))
    finalPosition.x * finalPosition.y
  }

  private def getFinalPosition(moves: Seq[Delta]): Delta = {
    moves.reduce(_ + _)
  }
}
