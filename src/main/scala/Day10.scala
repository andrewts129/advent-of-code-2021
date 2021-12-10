package io.andrewsmith.advent_of_code_2021

import Util.readLines

import scala.annotation.tailrec

object Day10 {
  case class Line(string: String) {
    def errorScore: Option[Int] = {
      firstIllegalCharacter match {
        case Some(')') => Some(3)
        case Some(']') => Some(57)
        case Some('}') => Some(1197)
        case Some('>') => Some(25137)
        case None => None
      }
    }

    private def firstIllegalCharacter: Option[Char] = {
      firstIllegalCharacter(0, List())
    }

    @tailrec
    private def firstIllegalCharacter(from: Int, stack: List[Char]): Option[Char] = {
      if (from >= string.length) {
        None
      } else {
        string(from) match {
          case opener @ ('(' | '{' | '[' | '<') => firstIllegalCharacter(from + 1, stack :+ opener)
          case closer @ (')' | '}' | ']' | '>') => if (closer == Line.equivalentCloser(stack.last)) {
            firstIllegalCharacter(from + 1, stack.dropRight(1))
          } else {
            Some(closer)
          }
        }
      }
    }
  }

  object Line {
    def equivalentCloser(opener: Char): Char = {
      Map(
        '(' -> ')',
        '{' -> '}',
        '[' -> ']',
        '<' -> '>'
      )(opener)
    }
  }

  def syntaxErrorScoreSum(fileName: String): Int = {
    readLines(fileName).map(Line(_)).flatMap(_.errorScore).sum
  }
}
