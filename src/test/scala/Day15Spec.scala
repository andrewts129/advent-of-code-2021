package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day15Spec extends AnyFunSpec {
  describe("#lowestPathRisk") {
    describe("sample input") {
      val input = "input/day15_sample.txt"
      val expectedOutput = 40

      it("returns the correct result") {
        assert(Day15.lowestPathRisk(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day15.txt"
      val expectedOutput = 748

      it("returns the correct result") {
        assert(Day15.lowestPathRisk(input) == expectedOutput)
      }
    }
  }

  describe("#lowestPathRiskExpanded") {
    describe("sample input") {
      val input = "input/day15_sample.txt"
      val expectedOutput = 315

      it("returns the correct result") {
        assert(Day15.lowestPathRiskExpanded(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day15.txt"
      val expectedOutput = 3045

      it("returns the correct result") {
        assert(Day15.lowestPathRiskExpanded(input) == expectedOutput)
      }
    }
  }
}
