package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day16Spec extends AnyFunSpec {
  describe("#sumOfVersionNumbers") {
    describe("sample input") {
      val input = "input/day16_sample_a.txt"
      val expectedOutput = 16

      it("returns the correct result") {
        assert(Day16.sumOfVersionNumbers(input) == expectedOutput)
      }
    }

    describe("sample input") {
      val input = "input/day16_sample_b.txt"
      val expectedOutput = 12

      it("returns the correct result") {
        assert(Day16.sumOfVersionNumbers(input) == expectedOutput)
      }
    }

    describe("sample input") {
      val input = "input/day16_sample_c.txt"
      val expectedOutput = 23

      it("returns the correct result") {
        assert(Day16.sumOfVersionNumbers(input) == expectedOutput)
      }
    }

    describe("sample input") {
      val input = "input/day16_sample_d.txt"
      val expectedOutput = 31

      it("returns the correct result") {
        assert(Day16.sumOfVersionNumbers(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day16.txt"
      val expectedOutput = -1

      it("returns the correct result") {
        assert(Day16.sumOfVersionNumbers(input) == expectedOutput)
      }
    }
  }
}
