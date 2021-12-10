package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day9Spec extends AnyFunSpec {
  describe("#sumOfRiskOfLowPoints") {
    describe("sample input") {
      val input = "input/day9_sample.txt"
      val expectedOutput = 15

      it("returns the correct result") {
        assert(Day9.sumOfRiskOfLowPoints(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day9.txt"
      val expectedOutput = 514

      it("returns the correct result") {
        assert(Day9.sumOfRiskOfLowPoints(input) == expectedOutput)
      }
    }
  }
}
