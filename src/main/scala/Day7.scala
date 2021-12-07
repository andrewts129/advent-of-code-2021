package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day7 {
  case class Crab(position: Int) {
    def fuelNeeded(target: Int, constantBurn: Boolean): Int = {
      val stepsNeeded = Math.abs(target - position)

      if (constantBurn) {
        stepsNeeded
      } else {
        (1 to stepsNeeded).sum
      }
    }
  }

  def minimumFuelConsumption(fileName: String, constantBurn: Boolean = true): Int = {
    minimumFuelConsumption(readLines(fileName).flatMap(parseCrabs), constantBurn)
  }

  private def minimumFuelConsumption(crabs: Seq[Crab], constantBurn: Boolean): Int = {
    val positions = crabs.map(_.position)
    val positionsToCheck = positions.min to positions.max

    positionsToCheck.map(minimumFuelConsumption(crabs, _, constantBurn)).min
  }

  private def minimumFuelConsumption(crabs: Seq[Crab], targetPosition: Int, constantBurn: Boolean): Int = {
    crabs.map(_.fuelNeeded(targetPosition, constantBurn)).sum
  }

  private def parseCrabs(string: String): Seq[Crab] = {
    string.split(",").map(position => Crab(position.toInt))
  }
}
