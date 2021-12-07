package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day7 {
  case class Crab(position: Int) {
    def fuelNeeded(target: Int): Int = {
      Math.abs(target - position)
    }
  }

  def minimumFuelConsumption(fileName: String): Int = {
    minimumFuelConsumption(readLines(fileName).flatMap(parseCrabs))
  }

  private def minimumFuelConsumption(crabs: Seq[Crab]): Int = {
    val positions = crabs.map(_.position).toSet

    positions.map(minimumFuelConsumption(crabs, _)).min
  }

  private def minimumFuelConsumption(crabs: Seq[Crab], targetPosition: Int): Int = {
    crabs.map(_.fuelNeeded(targetPosition)).sum
  }

  private def parseCrabs(string: String): Seq[Crab] = {
    string.split(",").map(position => Crab(position.toInt))
  }
}
