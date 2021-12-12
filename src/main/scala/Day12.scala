package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day12 {
  trait Cave {
    val label: String
  }

  object Cave {
    def parse(label: String): Cave = {
      if (label.head.isUpper) {
        BigCave(label)
      } else {
        SmallCave(label)
      }
    }
  }

  case class SmallCave(label: String) extends Cave

  case class BigCave(label: String) extends Cave

  case class Passage(start: Cave, end: Cave) {
    def connectingTo(cave: Cave): Boolean = {
      (start == cave) || (end == cave)
    }
  }

  case class PassageSet(passages: Set[Passage]) {
    def size: Int = passages.size

    def connectingTo(cave: Cave): PassageSet = {
      PassageSet(passages.filter(_.connectingTo(cave)))
    }

    def withoutPassagesConnectingTo(cave: Cave): PassageSet = {
      PassageSet(passages.filter(!_.connectingTo(cave)))
    }
  }

  case class CaveSystem(start: Cave, end: Cave, passageSet: PassageSet, revisitBurned: Boolean, visitedSmalls: Set[Cave] = Set()) {
    def numPossiblePaths: Int = {
      if (revisitBurned && visitedSmalls.contains(start)) {
        return 0
      }

      val pathsForward = passageSet.connectingTo(start)
      val directPaths = pathsForward.connectingTo(end)
      val possibleIndirectPaths = pathsForward.withoutPassagesConnectingTo(end)

      val followups = possibleIndirectPaths.passages.map {
        case Passage(a, b) =>
          val leavingStart = start.label == "start"
          val destination = if (a == start) b else a

          start match {
            case BigCave(_) => CaveSystem(
              destination,
              end,
              passageSet,
              revisitBurned,
              visitedSmalls
            )
            case SmallCave(_) => CaveSystem(
              destination,
              end,
              if (leavingStart) passageSet.withoutPassagesConnectingTo(start) else passageSet,
              revisitBurned = revisitBurned || (if (leavingStart) revisitBurned else visitedSmalls.contains(start)),
              visitedSmalls + start
            )
          }
      }.toList

      directPaths.size + followups.map(_.numPossiblePaths).sum
    }
  }

  object CaveSystem {
    def parse(lines: Seq[String], revisitEnabled: Boolean = false): CaveSystem = {
      val caves = lines.flatMap(_.split("-")).toSet.map {
        (label: String) => label -> Cave.parse(label)
      }.toMap
      val passages = lines.map(_.split("-")).map {
        case Array(startLabel, endLabel) => Passage(caves(startLabel), caves(endLabel))
      }.toSet

      CaveSystem(caves("start"), caves("end"), PassageSet(passages), !revisitEnabled)
    }
  }

  def numPossiblePaths(fileName: String): Int = {
    CaveSystem.parse(readLines(fileName)).numPossiblePaths
  }

  def numPossiblePathsWithRevisit(fileName: String): Int = {
    CaveSystem.parse(readLines(fileName), revisitEnabled = true).numPossiblePaths
  }
}
