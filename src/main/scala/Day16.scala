package io.andrewsmith.advent_of_code_2021

import Util.readLines

object Day16 {
  case class Packet(hexString: String) {
    private val binary = hexToBinary(hexString)

    def versionSum: Int = {
      version + subPackets.map(_.versionSum).sum
    }

    def excess: String = {

    }

    private def version: Int = {
      binaryToDecimal(binary.take(3))
    }

    private def packetType: Packet.PacketType = {
      packetTypeId match {
        case 4 => Packet.LITERAL
        case _ => Packet.OPERATOR
      }
    }

    private def packetTypeId: Int = {
      binaryToDecimal(binary.slice(3, 6))
    }

    private def lengthTypeId: Int = {
      binary(7).asDigit
    }

    private def subPackets: Seq[Packet] = {
      packetType match {
        case Packet.LITERAL => Seq()
        case Packet.OPERATOR => lengthTypeId match {
          case 0 =>
            val totalLengthOfSubpackets = binaryToDecimal(binary.slice(8, 23))
            ???
          case 1 =>
            val numberOfSubpackets = binaryToDecimal(binary.slice(8, 19))
            ???
        }
      }
    }
  }

  object Packet {
    sealed trait PacketType
    case object LITERAL extends PacketType
    case object OPERATOR extends PacketType
  }

  def sumOfVersionNumbers(fileName: String): Int = {
    Packet(readLines(fileName).head).versionSum
  }

  private def hexToBinary(hexString: String): String = {
    Integer.parseInt(hexString, 16).toBinaryString
  }

  private def binaryToHex(binaryString: String): String = {
    Integer.parseInt(binaryString, 2).toHexString
  }

  private def binaryToDecimal(binaryString: String): Int = {
    Integer.parseInt(binaryString, 2)
  }
}
