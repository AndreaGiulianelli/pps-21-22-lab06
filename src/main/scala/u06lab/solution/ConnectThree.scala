package u06lab.solution

import ConnectGame.*
import Player.*
import java.util.OptionalInt


object ConnectThree extends App:
  val connectTree = Connect(4)

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(connectTree.find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(connectTree.find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(connectTree.find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(connectTree.firstAvailableRow(List(), 0)) // Some(0)
  println(connectTree.firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(connectTree.firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(connectTree.firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(connectTree.firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  connectTree.printBoards(connectTree.placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  connectTree.printBoards(connectTree.placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 3: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  connectTree.computeAnyGame(O, 7).foreach { g =>
    connectTree.printBoards(g)
    println()
  }
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
