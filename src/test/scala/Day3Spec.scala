package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day3Spec extends AnyFunSpec {
  describe("#powerConsumption") {
    describe("sample input") {
      val input = "input/day3_sample.txt"
      val expectedOutput = 198

      it("returns the correct result") {
        assert(Day3.powerConsumption(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day3.txt"
      val expectedOutput = 3882564

      it("returns the correct result") {
        assert(Day3.powerConsumption(input) == expectedOutput)
      }
    }
  }

  describe("#lifeSupportRating") {
    describe("sample input") {
      val input = "input/day3_sample.txt"
      val expectedOutput = 230

      it("returns the correct result") {
        assert(Day3.lifeSupportRating(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day3.txt"
      val expectedOutput = 3385170

      it("returns the correct result") {
        assert(Day3.lifeSupportRating(input) == expectedOutput)
      }
    }
  }
}
