package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day1Spec extends AnyFunSpec {
  describe("#numberOfDepthIncreases") {
    describe("sample input") {
      val input = "input/day1_sample.txt"
      val expectedOutput = 7

      it("returns the correct result") {
        assert(Day1.numberOfDepthIncreases(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day1.txt"
      val expectedOutput = 1754

      it("returns the correct result") {
        assert(Day1.numberOfDepthIncreases(input) == expectedOutput)
      }
    }
  }

  describe("#numberOfDepthIncreasesSliding") {
    describe("sample input") {
      val input = "input/day1_sample.txt"
      val expectedOutput = 5

      it("returns the correct result") {
        assert(Day1.numberOfDepthIncreasesSliding(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day1.txt"
      val expectedOutput = 1789

      it("returns the correct result") {
        assert(Day1.numberOfDepthIncreasesSliding(input) == expectedOutput)
      }
    }
  }
}
