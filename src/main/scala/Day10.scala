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

    def completionScore: Option[Long] = {
      completionString match {
        case Some(str) => Some(Line.score(str))
        case None => None
      }
    }

    private def firstIllegalCharacter: Option[Char] = {
      parse(0, List()) match {
        case Left(illegalCharacter) => Some(illegalCharacter)
        case Right(_) => None
      }
    }

    @tailrec
    private def parse(from: Int, stack: List[Char]): Either[Char, List[Char]] = {
      if (from >= string.length) {
        Right(stack)
      } else {
        string(from) match {
          case opener @ ('(' | '{' | '[' | '<') => parse(from + 1, stack :+ opener)
          case closer @ (')' | '}' | ']' | '>') => if (closer == Line.equivalentCloser(stack.last)) {
            parse(from + 1, stack.dropRight(1))
          } else {
            Left(closer)
          }
        }
      }
    }

    private def completionString: Option[String] = {
      parse(0, List()) match {
        case Left(_) => None
        case Right(List()) => None
        case Right(remainingStack) => Some(remainingStack.reverse.map(Line.equivalentCloser).mkString)
      }
    }
  }

  object Line {
    private def equivalentCloser(opener: Char): Char = {
      Map(
        '(' -> ')',
        '{' -> '}',
        '[' -> ']',
        '<' -> '>'
      )(opener)
    }

    private def score(completionString: String): Long = {
      completionString.foldLeft(0L) {
        case (score, char) => (score * 5) + Map(
          ')' -> 1,
          ']' -> 2,
          '}' -> 3,
          '>' -> 4
        )(char)
      }
    }
  }

  def syntaxErrorScoreSum(fileName: String): Int = {
    readLines(fileName).map(Line(_)).flatMap(_.errorScore).sum
  }

  def middleCompletionScore(fileName: String): Long = {
    middle(readLines(fileName).map(Line(_)).flatMap(_.completionScore))
  }

  private def middle(seq: Seq[Long]): Long = {
    seq.sorted.apply(seq.size / 2)
  }
}
