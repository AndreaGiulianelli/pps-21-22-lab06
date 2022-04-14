package u06lab.solution

import u06lab.solution.AbstractConnect.Connect
import AbstractConnect.*
import Player.*

object TicTacToe extends App:
  val ticTacToe = new Connect(3){}

  ticTacToe.computeAnyGame(O, 7).foreach { g =>
    ticTacToe.printBoards(g)
    println()
  }