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

  case class Position(x: Int, y: Int, aim: Int) {
    def +(delta: AimDelta): Position = {
      Position(x + delta.x, y + delta.y(aim), aim + delta.aim)
    }
  }

  case class AimDelta(x: Int, y: Int => Int, aim: Int)

  object AimDelta {
    def parse(line: String): AimDelta = {
      line.split(" ", 2) match {
        case Array("forward", x) => AimDelta(x.toInt, { aim: Int => aim * x.toInt}, 0)
        case Array("down", y) => AimDelta(0, _ => 0, y.toInt)
        case Array("up", y) => AimDelta(0, _ => 0, y.toInt * -1)
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

  def productOfFinalPositionWithAim(fileName: String): Int = {
    productOfFinalPositionWithAim(readLines(fileName))
  }

  def productOfFinalPositionWithAim(lines: Seq[String]): Int = {
    val finalPosition = getFinalPositionWithAim(lines.map(AimDelta.parse))
    finalPosition.x * finalPosition.y
  }

  private def getFinalPosition(moves: Seq[Delta]): Delta = {
    moves.reduce(_ + _)
  }

  private def getFinalPositionWithAim(moves: Seq[AimDelta]): Position = {
    moves.foldLeft(Position(0, 0, 0))(_ + _)
  }
}
