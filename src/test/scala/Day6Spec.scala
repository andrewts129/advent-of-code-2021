package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day6Spec extends AnyFunSpec {
  describe("#numberOfFishAfter80Days") {
    describe("sample input") {
      val input = "input/day6_sample.txt"
      val expectedOutput = 5934

      it("returns the correct result") {
        assert(Day6.numberOfFishAfter80Days(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day6.txt"
      val expectedOutput = 350149

      it("returns the correct result") {
        assert(Day6.numberOfFishAfter80Days(input) == expectedOutput)
      }
    }
  }
}
