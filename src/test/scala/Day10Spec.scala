package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day10Spec extends AnyFunSpec {
  describe("#syntaxErrorScoreSum") {
    describe("sample input") {
      val input = "input/day10_sample.txt"
      val expectedOutput = 26397

      it("returns the correct result") {
        assert(Day10.syntaxErrorScoreSum(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day10.txt"
      val expectedOutput = 243939

      it("returns the correct result") {
        assert(Day10.syntaxErrorScoreSum(input) == expectedOutput)
      }
    }
  }

  describe("#middleCompletionScore") {
    describe("sample input") {
      val input = "input/day10_sample.txt"
      val expectedOutput = 288957

      it("returns the correct result") {
        assert(Day10.middleCompletionScore(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day10.txt"
      val expectedOutput = 2421222841L

      it("returns the correct result") {
        assert(Day10.middleCompletionScore(input) == expectedOutput)
      }
    }
  }
}
