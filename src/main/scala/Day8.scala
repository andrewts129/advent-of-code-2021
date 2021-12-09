package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day8 {
  case class Digit(chars: String) {
    def isEasyDigit: Boolean = {
      isOne || isFour || isSeven || isEight
    }

    def charSet: Set[Char] = chars.toCharArray.toSet

    def -(other: Digit): Set[Char] = {
      this.charSet -- other.charSet
    }

    def value(others: Set[Digit]): Int = {
      if (isOne) {
        1
      } else if (isFour) {
        4
      } else if (isSeven) {
        7
      } else if (isEight) {
        8
      } else if (chars.length == 5) {
        others.find(_.isFour) match {
          case Some(four) => (four - this).size match {
            case 2 => 2
            case 1 => others.find(digit => digit.isOne || digit.isSeven) match {
              case Some(oneOrSeven) => (oneOrSeven - this).size match {
                case 0 => 3
                case 1 => 5
              }
              case None => ???
            }
          }
          case None => ???
        }
      } else if (chars.length == 6) {
        others.find(_.isFour) match {
          case Some(four) => (four - this).size match {
            case 0 => 9
            case 1 => others.find(digit => digit.isOne || digit.isSeven) match {
              case Some(oneOrSeven) => (oneOrSeven - this).size match {
                case 0 => 0
                case 1 => 6
              }
              case None => ???
            }
          }
          case None => ???
        }
      } else {
        throw new RuntimeException
      }
    }

    private def isOne: Boolean = chars.length == 2

    private def isFour: Boolean = chars.length == 4

    private def isSeven: Boolean = chars.length == 3

    private def isEight: Boolean = chars.length == 7
  }

  case class Input(digits: Seq[Digit])

  object Input {
    def parse(string: String): Input = {
      Input(string.split(" ").map(Digit))
    }
  }

  case class Output(digits: Seq[Digit]) {
    def numEasyDigits: Int = {
      digits.count(_.isEasyDigit)
    }
  }

  object Output {
    def parse(string: String): Output = {
      Output(string.split(" ").map(Digit))
    }
  }

  case class Entry(input: Input, output: Output) {
    def numEasyDigitsInOutput: Int = output.numEasyDigits

    def outputValue: Int = {
      output.digits.map(_.value(allDigits).toString()).mkString.toInt
    }

    private def allDigits: Set[Digit] = {
      (input.digits ++ output.digits).toSet
    }
  }

  object Entry {
    def parse(line: String): Entry = {
      val Array(input, output) = line.split(" \\| ")

      Entry(Input.parse(input), Output.parse(output))
    }
  }

  def countEasyDigitOccurrencesInOutput(fileName: String): Int = {
    readLines(fileName).map(Entry.parse).map(_.numEasyDigitsInOutput).sum
  }

  def sumOfAllOutputValues(fileName: String): Int = {
    readLines(fileName).map(Entry.parse).map(_.outputValue).sum
  }
}
