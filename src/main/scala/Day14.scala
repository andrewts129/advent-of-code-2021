package io.andrewsmith.advent_of_code_2021

import fs2.{Pure, Stream}
import Util.{readLines, repartition}

object Day14 {
  case class Polymer(stream: Stream[Pure, Char]) {
    def apply(rules: PairInsertionRuleSet, n: Int): Polymer = {
      if (n == 0) {
        this
      } else {
        val newStream = stream.sliding(2).flatMap { chunk =>
          val head = chunk(0)
          val next = chunk(1)

          rules.ruleFor(head, next) match {
            case Some(rule) => Stream.emits(Seq(head, rule.insertion))
            case None => Stream.emit(head)
          }
        } ++ stream.last.map(_.get)

        Polymer(newStream)(rules, n - 1)
      }
    }

    def score: Int = {
      numElementsOfMostCommon - numElementsOfLeastCommon
    }

    private def numElementsOfMostCommon: Int = {
      elementCounts.values.max
    }

    private def numElementsOfLeastCommon: Int = {
      elementCounts.values.min
    }

    private def elementCounts: Map[Char, Int] = {
      stream.fold(
        Map[Char, Int]()
      ) {
        case (counts, char) => counts.updated(char, counts.getOrElse(char, 0) + 1)
      }.compile.toVector.head
    }
  }

  object Polymer {
    def apply(line: String): Polymer = {
      Polymer(Stream.emits(line.toCharArray))
    }
  }

  case class PairInsertionRuleSet(rules: Set[PairInsertionRule]) {
    def ruleFor(left: Char, right: Char): Option[PairInsertionRule] = {
      rules.find(rule => rule.left == left && rule.right == right)
    }
  }

  case class PairInsertionRule(left: Char, right: Char, insertion: Char)

  object PairInsertionRule {
    def apply(line: String): PairInsertionRule = {
      val Array(pattern, insertion) = line.split(" -> ")

      PairInsertionRule(pattern(0), pattern(1), insertion(0))
    }
  }

  def getPolymerScoreAfterTenIterations(fileName: String): Int = {
    val (initialPolymer, pairInsertionRules) = parseInput(fileName)

    initialPolymer(pairInsertionRules, 10).score
  }

  private def parseInput(fileName: String): (Polymer, PairInsertionRuleSet) = {
    val Seq(Seq(initialPolymerLine), pairInsertionLines) = repartition(readLines(fileName))

    (Polymer(initialPolymerLine), PairInsertionRuleSet(pairInsertionLines.map(PairInsertionRule.apply).toSet))
  }
}
