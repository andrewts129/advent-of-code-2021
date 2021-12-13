package io.andrewsmith.advent_of_code_2021

import org.scalatest.funspec._

import java.io.ByteArrayOutputStream

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

  describe("#printActivationCode") {
    describe("sample input") {
      val input = "input/day13_sample.txt"
      val expectedOutput = """*****
                             |*...*
                             |*...*
                             |*...*
                             |*****
                             |""".stripMargin

      it("returns the correct result") {
        val output = new ByteArrayOutputStream()

        Console.withOut(output) {
          Day13.printActivationCode(input)
        }

        assert(output.toString == expectedOutput)
      }
    }

    describe("real input") {
      val input = "input/day13.txt"
      val expectedOutput = """****..**..*..*.*..*.***..****..**..***.
                             |*....*..*.*..*.*.*..*..*.*....*..*.*..*
                             |***..*..*.****.**...*..*.***..*....*..*
                             |*....****.*..*.*.*..***..*....*....***.
                             |*....*..*.*..*.*.*..*.*..*....*..*.*...
                             |****.*..*.*..*.*..*.*..*.****..**..*...
                             |""".stripMargin

      it("returns the correct result") {
        val output = new ByteArrayOutputStream()

        Console.withOut(output) {
          Day13.printActivationCode(input)
        }

        assert(output.toString == expectedOutput)
      }
    }
  }
}
