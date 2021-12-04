package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day4Spec extends AnyFunSpec {
  describe("#scoreOfWinningBingoBoard") {
    describe("sample input") {
      val input = "input/day4_sample.txt"
      val expectedOutput = 4512

      it("returns the correct result") {
        assert(Day4.scoreOfWinningBingoBoard(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day4.txt"
      val expectedOutput = 8580

      it("returns the correct result") {
        assert(Day4.scoreOfWinningBingoBoard(input) == expectedOutput)
      }
    }
  }

  describe("#scoreOfLastWinningBingoBoard") {
    describe("sample input") {
      val input = "input/day4_sample.txt"
      val expectedOutput = 1924

      it("returns the correct result") {
        assert(Day4.scoreOfLastWinningBingoBoard(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day4.txt"
      val expectedOutput = 9576

      it("returns the correct result") {
        assert(Day4.scoreOfLastWinningBingoBoard(input) == expectedOutput)
      }
    }
  }
}
