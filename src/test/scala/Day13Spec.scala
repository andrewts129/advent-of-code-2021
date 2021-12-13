package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day13Spec extends AnyFunSpec {
  describe("#dotsVisibleAfterFirstFold") {
    describe("sample input") {
      val input = "input/day13_sample.txt"
      val expectedOutput = 17

      it("returns the correct result") {
        assert(Day13.dotsVisibleAfterFirstFold(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day13.txt"
      val expectedOutput = 827

      it("returns the correct result") {
        assert(Day13.dotsVisibleAfterFirstFold(input) == expectedOutput)
      }
    }
  }
}
