package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day8Spec extends AnyFunSpec {
  describe("#countEasyDigitOccurrencesInOutput") {
    describe("sample input") {
      val input = "input/day8_sample.txt"
      val expectedOutput = 26

      it("returns the correct result") {
        assert(Day8.countEasyDigitOccurrencesInOutput(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day8.txt"
      val expectedOutput = 514

      it("returns the correct result") {
        assert(Day8.countEasyDigitOccurrencesInOutput(input) == expectedOutput)
      }
    }
  }

  describe("#sumOfAllOutputValues") {
    describe("sample input") {
      val input = "input/day8_sample.txt"
      val expectedOutput = 61229

      it("returns the correct result") {
        assert(Day8.sumOfAllOutputValues(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day8.txt"
      val expectedOutput = 1012272

      it("returns the correct result") {
        assert(Day8.sumOfAllOutputValues(input) == expectedOutput)
      }
    }
  }
}
