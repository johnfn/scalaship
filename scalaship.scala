object battlescala {
  class Point(xc: Int, yc: Int) {
    var x: Int = xc
    var y: Int = yc
    override def toString(): String = "(" + x + ", " + y + ")";
  }

  // Put the print() method on objects of type Array[Array[T]]
  class PrintArray[T](at: Array[Array[T]]) {
    def print() = at.map(line => println(line.mkString("")))
  }
  implicit def array2foo[T](at: Array[Array[T]]) = new PrintArray(at)

  val SHOT_NOT_TAKEN = "."
  val SHOT_MISS      = "~"
  val SHOT_HIT       = "X"

  val BOARD_EMPTY = "."
  val BOARD_SHIP  = "X"

  def readCoords(playerNum: Int): List[Point] = {
    println("Player "+playerNum+": Enter x1 y1 x2 y2. Don't do anything clever because Scala will probably just explode.")
    val coords = readLine().split(" ").map(_.toInt)
    List(new Point(coords(0), coords(1)), new Point(coords(2), coords(3)))
  }

  def takeGuess(playerNum: Int): Array[Int] = {
    println("Player "+playerNum+": Take a guess.")
    val result:Array[Int] = readLine().split(" ").map(_.toInt)
    assert(result.length == 2, "Improper input")
    result
  }

  def emptyBoard(): Array[Array[String]] = {
    Array.ofDim[String](10, 10).map(line => line.map(_ => "."))
  }

  def calcLife(board: Array[Array[String]]): Int = {
    board.flatten.filter(_ == BOARD_SHIP).length
  }

  def gameLoop(boards: List[Array[Array[String]]]): Unit = {
    var gameOver:Boolean = false
    var currentPlayer:Int = 0
    var shots = List(emptyBoard(), emptyBoard())
    var hitsLeft = Array(calcLife(boards(1)), calcLife(boards(0))) //each counts down the number of hits on the other

    while (!gameOver) {
      val guess = takeGuess(currentPlayer)
      val otherPlayer = (currentPlayer + 1) % 2
      if (shots(currentPlayer)(guess(0))(guess(1)) == SHOT_NOT_TAKEN) {
        if (boards(otherPlayer)(guess(0))(guess(1)) == BOARD_SHIP) {
          println("Hit!")
          hitsLeft(currentPlayer) -= 1
          if (hitsLeft(currentPlayer) <= 0) {
            println("You win!");
            gameOver = true
          }
        } else {
          println("Miss.")
        }
        shots(currentPlayer)(guess(0))(guess(1)) = SHOT_HIT
        currentPlayer = otherPlayer
      } else {
        println("You already guessed that, silly.")
      }
    }
  }

  def placeShip(boards: List[Array[Array[String]]], playerNum: Int, coords: List[Point]): List[Array[Array[String]]] = {
    if (coords(0).x == coords(1).x) {
      val x = coords(0).x
      val y1 = coords(0).y
      val y2 = coords(1).y
      for (y <- y1 to y2 by (if (y1 > y2) -1 else 1)) {
        boards(playerNum)(x)(y) = BOARD_SHIP
      }
    } else {
      val y = coords(0).y
      var x1 = coords(0).x
      var x2 = coords(1).x
      for (x <- x1 to x2 by (if (x1 > x2) -1 else 1)) {
        boards(playerNum)(x)(y) = BOARD_SHIP
      }
    }
    boards
  }

  def main(args: Array[String]): Unit = {
    var boards = List(emptyBoard(), emptyBoard())
    boards.head.print()
    for (playerNum <- 0 until 2) {
      val coords = readCoords(playerNum)
      boards = placeShip(boards, playerNum, coords)
    }
    boards(0).print()
    boards(1).print()
    gameLoop(boards)
  }
}
