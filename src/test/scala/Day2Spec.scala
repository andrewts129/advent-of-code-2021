package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day2Spec extends AnyFunSpec {
  describe("#productOfFinalPosition") {
    describe("sample input") {
      val input = "input/day2_sample.txt"
      val expectedOutput = 150

      it("returns the correct result") {
        assert(Day2.productOfFinalPosition(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day2.txt"
      val expectedOutput = 1893605

      it("returns the correct result") {
        assert(Day2.productOfFinalPosition(input) == expectedOutput)
      }
    }
  }
}
