package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

class Day7Spec extends AnyFunSpec {
  describe("#minimumFuelConsumption") {
    describe("sample input") {
      val input = "input/day7_sample.txt"
      val expectedOutput = 37

      it("returns the correct result") {
        assert(Day7.minimumFuelConsumption(input) == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day7.txt"
      val expectedOutput = 336721

      it("returns the correct result") {
        assert(Day7.minimumFuelConsumption(input) == expectedOutput)
      }
    }

    describe("with non-constant fuel consumption") {
      describe("sample input") {
        val input = "input/day7_sample.txt"
        val expectedOutput = 168

        it("returns the correct result") {
          assert(Day7.minimumFuelConsumption(input, constantBurn = false) == expectedOutput)
        }
      }

      describe("real input") {
        val input = "input/day7.txt"
        val expectedOutput = 91638945

        it("returns the correct result") {
          assert(Day7.minimumFuelConsumption(input, constantBurn = false) == expectedOutput)
        }
      }
    }
  }
}
