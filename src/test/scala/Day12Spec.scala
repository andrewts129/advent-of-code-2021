package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day12Spec extends AnyFunSpec {
  describe("#numPossiblePaths") {
    describe("sample input") {
      val input = "input/day12_sample.txt"
      val expectedOutput = 10

      it("returns the correct result") {
        assert(Day12.numPossiblePaths(input) == expectedOutput)
      }
    }

    describe("second sample input") {
      val input = "input/day12_sample_b.txt"
      val expectedOutput = 19

      it("returns the correct result") {
        assert(Day12.numPossiblePaths(input) == expectedOutput)
      }
    }

    describe("third sample input") {
      val input = "input/day12_sample_c.txt"
      val expectedOutput = 226

      it("returns the correct result") {
        assert(Day12.numPossiblePaths(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day12.txt"
      val expectedOutput = 4549

      it("returns the correct result") {
        assert(Day12.numPossiblePaths(input) == expectedOutput)
      }
    }
  }
}
