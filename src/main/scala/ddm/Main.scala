package ddm

import ddm.Cell.State.{Miss, _Hit}
import ddm.Reel.Value.{Joker, Number}

object Main extends App {
  test(FiveByFive)

  private def test(testCase: TestCase): Unit = {
    val evaluator = new Evaluator(testCase.boardSize, testCase.board, testCase.reels)
    println(s"The expected value of a spin is ${evaluator.calculateSpinValue()}")
  }
  
  private def p(x: Int, y: Int): Position =
    Position(x, y)
    
  private def n(i: Int): Number =
    Number(i)

  private def r(values: Reel.Value*): Reel =
    Reel(values.toList)

  private trait TestCase {
    def boardSize: Int
    def board: Board
    def reels: List[Reel]
  }

  private object TwoByTwo1 extends TestCase {
    val boardSize: Int = 2

    val board: Board = Board(Map(
      p(1, 1) -> Cell(1, _Hit), p(2, 1) -> Cell(3, Miss),
      p(1, 2) -> Cell(2, _Hit), p(2, 2) -> Cell(4, _Hit),
    ))

    val reels: List[Reel] = List(
      r(n(1), n(2)),
      r(n(3), n(4)),
    )
  }

  private object TwoByTwo2 extends TestCase {
    val boardSize: Int = 2

    val board: Board = Board(Map(
      p(1, 1) -> Cell(1, Miss), p(2, 1) -> Cell(3, Miss),
      p(1, 2) -> Cell(2, _Hit), p(2, 2) -> Cell(4, Miss),
    ))

    val reels: List[Reel] = List(
      r(n(1), n(2)),
      r(n(3), n(4)),
    )
  }

  private object ThreeByThree1 extends TestCase {
    val boardSize: Int = 3

    val board: Board = Board(Map(
      p(1, 1) -> Cell(1, Miss), p(2, 1) -> Cell(4, _Hit), p(3, 1) -> Cell(7, Miss),
      p(1, 2) -> Cell(2, Miss), p(2, 2) -> Cell(5, _Hit), p(3, 2) -> Cell(8, Miss),
      p(1, 3) -> Cell(3, Miss), p(2, 3) -> Cell(6, _Hit), p(3, 3) -> Cell(9, Miss),
    ))

    val reels: List[Reel] = List(
      r(n(1), n(2), n(3)),
      r(n(4), n(5), n(6)),
      r(n(7), n(8), n(9)),
    )
  }

  private object ThreeByThree2 extends TestCase {
    val boardSize: Int = 3

    val board: Board = Board(Map(
      p(1, 1) -> Cell(1, Miss), p(2, 1) -> Cell(4, _Hit), p(3, 1) -> Cell(7, Miss),
      p(1, 2) -> Cell(2, Miss), p(2, 2) -> Cell(5, _Hit), p(3, 2) -> Cell(8, Miss),
      p(1, 3) -> Cell(3, Miss), p(2, 3) -> Cell(6, _Hit), p(3, 3) -> Cell(9, Miss),
    ))

    val reels: List[Reel] = List(
      r(n(1), n(2), n(3)),
      r(n(4), n(5), n(6)),
      r(n(7), n(8), n(9), n(9)),
    )
  }

  private object ThreeByThree3 extends TestCase {
    val boardSize: Int = 3

    val board: Board = Board(Map(
      p(1, 1) -> Cell(1, Miss), p(2, 1) -> Cell(4, _Hit), p(3, 1) -> Cell(7, Miss),
      p(1, 2) -> Cell(2, Miss), p(2, 2) -> Cell(5, _Hit), p(3, 2) -> Cell(8, Miss),
      p(1, 3) -> Cell(3, Miss), p(2, 3) -> Cell(6, _Hit), p(3, 3) -> Cell(9, Miss),
    ))

    val reels: List[Reel] = List(
      r(n(1), n(2), n(3)),
      r(n(4), n(5), n(6)),
      r(n(7), n(8), n(9), Joker),
    )
  }

  private object ThreeByThree4 extends TestCase {
    val boardSize: Int = 3

    val board: Board = Board(Map(
      p(1, 1) -> Cell(1, Miss), p(2, 1) -> Cell(4, _Hit), p(3, 1) -> Cell(7, Miss),
      p(1, 2) -> Cell(2, Miss), p(2, 2) -> Cell(5, _Hit), p(3, 2) -> Cell(8, Miss),
      p(1, 3) -> Cell(3, Miss), p(2, 3) -> Cell(6, _Hit), p(3, 3) -> Cell(9, Miss),
    ))

    val reels: List[Reel] = List(
      r(n(1), n(2), n(3)),
      r(n(4), n(5), n(6)),
      r(n(7), n(8), n(9), Joker, Joker),
    )
  }

  private object ThreeByThree5 extends TestCase {
    val boardSize: Int = 3

    val board: Board = Board(Map(
      p(1, 1) -> Cell(1, Miss), p(2, 1) -> Cell(4, Miss), p(3, 1) -> Cell(7, Miss),
      p(1, 2) -> Cell(2, Miss), p(2, 2) -> Cell(5, _Hit), p(3, 2) -> Cell(8, Miss),
      p(1, 3) -> Cell(3, Miss), p(2, 3) -> Cell(6, _Hit), p(3, 3) -> Cell(9, Miss),
    ))

    val reels: List[Reel] = List(
      r(n(1), n(2), n(3), Joker),
      r(n(4), n(5), n(6)),
      r(n(7), n(8), n(9), Joker),
    )
  }

  private object FiveByFive extends TestCase {
    val boardSize: Int = 5

    val board: Board = Board(Map(
      p(1, 1) -> Cell(1, _Hit), p(2, 1) -> Cell( 6, Miss), p(3, 1) -> Cell(11, _Hit), p(4, 1) -> Cell(16, Miss), p(5, 1) -> Cell(21, Miss),
      p(1, 2) -> Cell(2, Miss), p(2, 2) -> Cell( 7, _Hit), p(3, 2) -> Cell(12, Miss), p(4, 2) -> Cell(17, _Hit), p(5, 2) -> Cell(22, _Hit),
      p(1, 3) -> Cell(3, _Hit), p(2, 3) -> Cell( 8, _Hit), p(3, 3) -> Cell(13, _Hit), p(4, 3) -> Cell(18, Miss), p(5, 3) -> Cell(23, Miss),
      p(1, 4) -> Cell(4, Miss), p(2, 4) -> Cell( 9, _Hit), p(3, 4) -> Cell(14, _Hit), p(4, 4) -> Cell(19, _Hit), p(5, 4) -> Cell(24, _Hit),
      p(1, 5) -> Cell(5, Miss), p(2, 5) -> Cell(10, _Hit), p(3, 5) -> Cell(15, Miss), p(4, 5) -> Cell(20, _Hit), p(5, 5) -> Cell(25, Miss),
    ))

    val reels: List[Reel] = List(
      r(n( 1), n( 2), n( 3), n( 4), n( 5), Joker),
      r(n( 6), n( 7), n( 8), n( 9), n(10), Joker),
      r(n(11), n(12), n(13), n(14), n(15), Joker),
      r(n(16), n(17), n(18), n(19), n(20), Joker),
      r(n(21), n(22), n(23), n(24), n(25), Joker),
    )
  }
}
