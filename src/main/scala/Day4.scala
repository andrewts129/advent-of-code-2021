package io.andrewsmith.advent_of_code_2021

import Util.{readLines, repartition}

import scala.annotation.tailrec

object Day4 {
  case class BingoBoard(rows: Seq[Seq[Int]], marked: Set[Int] = Set()) {
    def mark(number: Int): BingoBoard = {
      BingoBoard(rows, marked + number)
    }

    def isWinner: Boolean = {
      hasWinningRow || hasWinningColumn
    }

    def sumOfUnmarked: Int = {
      rows.flatten.filter(!marked.contains(_)).sum
    }

    private def hasWinningRow: Boolean = {
      rows.exists(winningSequence)
    }

    private def hasWinningColumn: Boolean = {
      rows.head.indices.exists(winningColumn)
    }

    private def winningColumn(columnIndex: Int): Boolean = {
      val column = rows.map(_(columnIndex))
      winningSequence(column)
    }

    private def winningSequence(numbers: Seq[Int]): Boolean = {
      numbers.forall(marked.contains)
    }
  }

  object BingoBoard {
    def parse(lines: Seq[String]): BingoBoard = {
      val numbers = lines.map(
        _.strip().replaceAll("\\s+", " ").split(" ").map(_.toInt).toVector
      )

      BingoBoard(numbers)
    }
  }

  def scoreOfWinningBingoBoard(fileName: String): Int = {
    val (numbers, boards) = parseInput(fileName)

    scoreOfWinningBingoBoard(numbers, boards)
  }

  def scoreOfLastWinningBingoBoard(fileName: String): Int = {
    val (numbers, boards) = parseInput(fileName)

    scoreOfLastWinningBingoBoard(numbers, boards)
  }

  private def parseInput(fileName: String): (Seq[Int], Seq[BingoBoard]) = {
    val blocks = repartition(readLines(fileName))

    val numberList = parseNumberList(blocks.head.head)
    val bingoBoards = blocks.tail.map(BingoBoard.parse)

    (numberList, bingoBoards)
  }

  private def parseNumberList(numbers: String): Seq[Int] = {
    numbers.split(",").map(_.toInt)
  }

  @tailrec
  private def scoreOfWinningBingoBoard(numbers: Seq[Int], boards: Seq[BingoBoard]): Int = {
    val newBoards = boards.map(_.mark(numbers.head))

    winningBoard(newBoards) match {
      case Some(winner) => numbers.head * winner.sumOfUnmarked
      case None => scoreOfWinningBingoBoard(numbers.tail, newBoards)
    }
  }

  private def winningBoard(boards: Seq[BingoBoard]): Option[BingoBoard] = {
    boards.find(_.isWinner)
  }

  @tailrec
  private def scoreOfLastWinningBingoBoard(numbers: Seq[Int], boards: Seq[BingoBoard]): Int = {
    val newBoards = boards.map(_.mark(numbers.head))

    newBoards.filter(!_.isWinner) match {
      case Seq(lastRemainingBoard) => scoreOfWinningBingoBoard(numbers.tail, Seq(lastRemainingBoard))
      case remainingBoards => scoreOfLastWinningBingoBoard(numbers.tail, remainingBoards)
    }
  }
}
