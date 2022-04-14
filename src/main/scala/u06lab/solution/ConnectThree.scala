package u06lab.solution

import java.util.OptionalInt

object ConnectThree extends App:
  val bound = 3
  enum Player:
    case X, O // Two player: X and O
    def other: Player = this match // Return the "id" of the other player in the match
      case X => O
      case _ => X

  // Represent the place of one disk associated to the player that left it
  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(d => d.x == x && d.y == y).map(_.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    val pos = (0 to bound).diff(board.filter(_.x == x).map(_.y));
    Option.when(!pos.isEmpty)(pos.min)

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound // Iterate columns to choice
      y = firstAvailableRow(board, x)
      if !y.isEmpty
    yield
      Disk(x, y.get, player) +:board

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = (moves match
    case 0 => LazyList[Game](List(List())) // LazyList[Game](List[Board](List[Disk](Disk, ...), ...), ...)
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        lastBoard = game.head
        nextBoard <- placeAnyDisk(lastBoard, player).reverse // reverse in order to have the same output sequence as described below
      yield
        if !isGameOver(lastBoard) then // if internal in the for won't work, why?
          nextBoard +: game
        else
          game).distinct

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  private def isGameOver(board: Board): Boolean =
      // Inneficient, for educational purposes.
      var status = false
      for
        disk <- board
        x = disk.x
        y = disk.y
        player = disk.player
        dx <- -1 to 1
        dy <- -1 to 1
        if !(dx == 0 && dy == 0)
        if x + 2 * dx <= bound && y + 2 * dy <= bound
        if find(board, x + dx, y + dy).filter(_ == player).isDefined &&
          find(board, x + 2 * dx, y + 2 * dy).filter(_ == player).isDefined
      do
        status = true
      status

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 3: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 7).foreach { g =>
    printBoards(g)
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
