package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day11Spec extends AnyFunSpec {
  describe("#numFlashesThrough100Steps") {
    describe("sample input") {
      val input = "input/day11_sample.txt"
      val expectedOutput = 1656

      it("returns the correct result") {
        assert(Day11.numFlashesThrough100Steps(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day11.txt"
      val expectedOutput = 1697

      it("returns the correct result") {
        assert(Day11.numFlashesThrough100Steps(input) == expectedOutput)
      }
    }
  }

  describe("#firstStepWithAllFlashing") {
    describe("sample input") {
      val input = "input/day11_sample.txt"
      val expectedOutput = 195

      it("returns the correct result") {
        assert(Day11.firstStepWithAllFlashing(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day11.txt"
      val expectedOutput = 344

      it("returns the correct result") {
        assert(Day11.firstStepWithAllFlashing(input) == expectedOutput)
      }
    }
  }
}
