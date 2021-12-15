package io.andrewsmith.advent_of_code_2021

import Util.{readLines, repartition}

object Day14 {
  case class Polymer(pairs: Map[(Char, Char), Long], last: Char) {
    def apply(rules: PairInsertionRuleSet, n: Int): Polymer = {
      if (n == 0) {
        this
      } else {
        val ungrouped = pairs.toVector.flatMap {
          case (pair, count) => rules.ruleFor(pair) match {
            case Some(rule) => List(
              (pair._1, rule.insertion) -> count,
              (rule.insertion, pair._2) -> count
            )
            case None => List(pair -> count)
          }
        }

        val newPairs = ungrouped.groupMapReduce(_._1)(_._2)(_ + _)
        Polymer(newPairs, last).apply(rules, n - 1)
      }
    }

    def score: Long = {
      numElementsOfMostCommon - numElementsOfLeastCommon
    }

    private def numElementsOfMostCommon: Long = {
      elementCounts.values.max
    }

    private def numElementsOfLeastCommon: Long = {
      elementCounts.values.min
    }

    private def elementCounts: Map[Char, Long] = {
      val pairCounts = pairs.toVector.groupMapReduce(_._1._1)(_._2)(_ + _)
      pairCounts.updated(last, pairCounts(last) + 1)
    }
  }

  object Polymer {
    def apply(line: String): Polymer = {
      val pairs = line.sliding(2).toVector
      val pairCounts = pairs.groupMapReduce(pair => (pair(0), pair(1)))(_ => 1L)(_ + _)

      Polymer(pairCounts, line.last)
    }
  }

  case class PairInsertionRuleSet(rules: Set[PairInsertionRule]) {
    def ruleFor(pair: (Char, Char)): Option[PairInsertionRule] = {
      ruleFor(pair._1, pair._2)
    }

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

  def getPolymerScoreAfterTenIterations(fileName: String): Long = {
    val (initialPolymer, pairInsertionRules) = parseInput(fileName)

    initialPolymer(pairInsertionRules, 10).score
  }

  def getPolymerScoreAfterFortyIterations(fileName: String): Long = {
    val (initialPolymer, pairInsertionRules) = parseInput(fileName)

    initialPolymer(pairInsertionRules, 40).score
  }

  private def parseInput(fileName: String): (Polymer, PairInsertionRuleSet) = {
    val Seq(Seq(initialPolymerLine), pairInsertionLines) = repartition(readLines(fileName))

    (Polymer(initialPolymerLine), PairInsertionRuleSet(pairInsertionLines.map(PairInsertionRule.apply).toSet))
  }
}
