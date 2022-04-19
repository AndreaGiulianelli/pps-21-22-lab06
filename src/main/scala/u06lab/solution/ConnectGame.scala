package u06lab.solution

object ConnectGame:
  enum Player:
    case X, O // Two player: X and O
    def other: Player = this match // Return the "id" of the other player in the match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  type Board = Seq[Disk]
  type Game = Seq[Board]

  trait Connect:
    def find(board: Board, x: Int, y: Int): Option[Player]
    def firstAvailableRow(board: Board, x: Int): Option[Int]
    def placeAnyDisk(board: Board, player: Player): Seq[Board]
    def computeAnyGame(player: Player, moves: Int): LazyList[Game]
    def printBoards(game: Seq[Board]): Unit

  object Connect:
    def apply(b: Int): Connect = ConnectImpl(b)
    private class ConnectImpl(b: Int) extends Connect:
      val bound = b - 1
      override def find(board: Board, x: Int, y: Int): Option[Player] = board.find(d => d.x == x && d.y == y).map(_.player)

      override def firstAvailableRow(board: Board, x: Int): Option[Int] =
        val pos = (0 to bound).diff(board.filter(_.x == x).map(_.y));
        Option.when(!pos.isEmpty)(pos.min)

      override def placeAnyDisk(board: Board, player: Player): Seq[Board] =
        for
          x <- 0 to bound // Iterate columns to choice
          y = firstAvailableRow(board, x)
          if !y.isEmpty
        yield
          Disk(x, y.get, player) +: board

      override def computeAnyGame(player: Player, moves: Int): LazyList[Game] = (moves match
        case 0 => LazyList[Game](List(List())) // LazyList[Game](List[Board](List[Disk](Disk, ...), ...), ...)
        case _ =>
          for
            game <- computeAnyGame(player.other, moves - 1)
            lastBoard = game.head
            nextBoard <- placeAnyDisk(lastBoard, player).reverse // reverse in order to have the same output sequence as described below (it's optional)
          yield
            if !isGameOver(lastBoard) then
              nextBoard +: game
            else
              game).distinct

      override def printBoards(game: Seq[Board]): Unit =
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

