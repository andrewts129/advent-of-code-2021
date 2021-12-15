package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

import java.io.ByteArrayOutputStream

class Day14Spec extends AnyFunSpec {
  describe("#getPolymerScoreAfterTenIterations") {
    describe("sample input") {
      val input = "input/day14_sample.txt"
      val expectedOutput = 1588

      it("returns the correct result") {
        assert(Day14.getPolymerScoreAfterTenIterations(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day14.txt"
      val expectedOutput = 3213

      it("returns the correct result") {
        assert(Day14.getPolymerScoreAfterTenIterations(input) == expectedOutput)
      }
    }
  }
}
