package u06lab.solution

import ConnectGame.*
import Player.*

object TicTacToe extends App:
  val ticTacToe = Connect(3)

  ticTacToe.computeAnyGame(O, 7).foreach { g =>
    ticTacToe.printBoards(g)
    println()
  }