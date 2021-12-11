package io.andrewsmith.advent_of_code_2021

import Util.{Grid, parseGridValues, readLines}

object Day11 {
  case class Octopus(energyLevel: Option[Int], x: Int, y: Int) {
    def step: Octopus = {
      energyLevel match {
        case Some(value) => Octopus(Some(value + 1), x, y)
        case None => Octopus(Some(1), x, y)
      }
    }

    def flash: Octopus = {
      Octopus(None, x, y)
    }

    def readyToFlash: Boolean = {
      energyLevel match {
        case Some(value) => value > 9
        case None => false
      }
    }

    def flashed: Octopus = {
      energyLevel match {
        case Some(value) => Octopus(Some(value + 1), x, y)
        case None => this
      }
    }
  }

  case class OctopusGrid(rows: Seq[Seq[Octopus]]) extends Grid[Octopus] {
    def countFlashes(steps: Int): Int = {
      if (steps == 0) {
        0
      } else {
        val (next, numFlashesInStep) = step
        numFlashesInStep + next.countFlashes(steps - 1)
      }
    }

    private def step: (OctopusGrid, Int) = {
      val afterEnergyIncrement = rows.map(row => row.map(_.step))
      OctopusGrid(afterEnergyIncrement).processFlashes
    }

    private def processFlashes: (OctopusGrid, Int) = {
      octopusReadyToFlash match {
        case Some(flashingOctopus) =>
          val neighbors = neighborsOf(flashingOctopus.x, flashingOctopus.y)

          val newRows = this.mapValues {
            octopus => if (octopus == flashingOctopus) {
              octopus.flash
            } else if (neighbors.contains(octopus)) {
              octopus.flashed
            } else {
              octopus
            }
          }

          val (afterFlashes, numFlashes) = OctopusGrid(newRows).processFlashes

          (afterFlashes, numFlashes + 1)
        case None => (this, 0)
      }
    }

    private def octopusReadyToFlash: Option[Octopus] = {
      values.find(_.readyToFlash)
    }

    private def neighborsOf(x: Int, y: Int): Set[Octopus] = {
      Set(
        this(x, y + 1),
        this(x, y - 1),
        this(x + 1, y + 1),
        this(x + 1, y - 1),
        this(x + 1, y),
        this(x - 1, y + 1),
        this(x - 1, y - 1),
        this(x - 1, y)
      ).flatten
    }
  }

  object OctopusGrid {
    def parse(lines: Seq[String]): OctopusGrid = {
      OctopusGrid(
        parseGridValues[Octopus](lines, (cell, x, y) => Octopus(Some(cell.toInt), x, y))
      )
    }
  }

  def numFlashesThrough100Steps(fileName: String): Int = {
    OctopusGrid.parse(readLines(fileName)).countFlashes(100)
  }
}
