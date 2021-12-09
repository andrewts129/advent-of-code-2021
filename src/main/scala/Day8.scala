package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day8 {
  case class Digit(chars: String) {
    def isEasyDigit: Boolean = {
      isOne || isFour || isSeven || isEight
    }

    private def isOne: Boolean = chars.length == 2

    private def isFour: Boolean = chars.length == 4

    private def isSeven: Boolean = chars.length == 3

    private def isEight: Boolean = chars.length == 7
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

  case class Entry(output: Output) {
    def numEasyDigitsInOutput: Int = output.numEasyDigits
  }

  object Entry {
    def parse(line: String): Entry = {
      val Array(_, output) = line.split(" \\| ")

      Entry(Output.parse(output))
    }
  }

  def countEasyDigitOccurrencesInOutput(fileName: String): Int = {
    readLines(fileName).map(Entry.parse).map(_.numEasyDigitsInOutput).sum
  }
}
