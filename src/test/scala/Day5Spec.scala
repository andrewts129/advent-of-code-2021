package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day5Spec extends AnyFunSpec {
  describe("#numberOfOverlapPoints") {
    describe("sample input") {
      val input = "input/day5_sample.txt"
      val expectedOutput = 5

      it("returns the correct result") {
        assert(Day5.numberOfOverlapPoints(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day5.txt"
      val expectedOutput = 8622

      it("returns the correct result") {
        assert(Day5.numberOfOverlapPoints(input) == expectedOutput)
      }
    }

    describe("with diagonals") {
      describe("sample input") {
        val input = "input/day5_sample.txt"
        val expectedOutput = 12

        it("returns the correct result") {
          assert(Day5.numberOfOverlapPoints(input, includeDiagonals = true) == expectedOutput)
        }
      }

      describe("real input") {
        val input = "input/day5.txt"
        val expectedOutput = 22037

        it("returns the correct result") {
          assert(Day5.numberOfOverlapPoints(input, includeDiagonals = true) == expectedOutput)
        }
      }
    }
  }
}
