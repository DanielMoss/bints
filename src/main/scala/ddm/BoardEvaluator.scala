package ddm

import scala.annotation.tailrec
import scala.collection.mutable

object BoardEvaluator {
  final case class Result(raw: Map[Int, List[(EncodedPositions, Set[PrizeLine])]]) {
    def get(length: Int): List[(EncodedPositions, Set[PrizeLine])] =
      raw.getOrElse(length, List.empty)
  }
}

final class BoardEvaluator(boardSize: Int, codec: Codec, allPrizeLines: Set[PrizeLine]) {
  // Searches for evolutions of the board consisting of up to boardSize positions,
  // where such evolutions result in acquiring new prize lines.
  def findRewardingEvolutions(board: Board): BoardEvaluator.Result = {
    val (hitPositions, missingPositions) = {
      val (hits, misses) = board.cells.partition { case (_, cell) => cell.state == Cell.State._Hit }
      (hits.keys, misses.keys)
    }

    val encodedBoard = codec.encode(hitPositions)
    val availablePrizeLines = allPrizeLines.filter(!hasHit(encodedBoard, _))

    BoardEvaluator.Result(
      potentialBoardEvolutions(missingPositions.toList).map { case (length, evolutions) =>
        (length, filterToRewardingEvolutions(encodedBoard, availablePrizeLines, evolutions))
      }
    )
  }

  private def hasHit(board: EncodedPositions, prizeLine: PrizeLine): Boolean =
    board.contains(prizeLine.positions)

  // Calculates the set of all possible evolutions to the board, by considering all potential
  // combinations of hitting the available positions, given that we hit up to n new positions
  // at a time, where n is equal to the board size.
  private def potentialBoardEvolutions(availablePositions: List[Position]): Map[Int, List[EncodedPositions]] =
    potentialBoardEvolutionsHelper(
      mutable.Map(0 -> List.empty),
      availablePositions.map(p => codec.encode(p)),
      maxLength = boardSize
    )

  @tailrec
  private def potentialBoardEvolutionsHelper(
    acc: mutable.Map[Int, List[EncodedPositions]],
    remaining: List[EncodedPositions],
    maxLength: Int
  ): Map[Int, List[EncodedPositions]] =
    remaining match {
      case Nil => acc.toMap
      case position :: tail =>
        (maxLength - 1).to(1, -1).foreach { length =>
          acc.get(length).foreach { positionSets =>
            val setsWithNewPosition = positionSets.map(_ + position)
            val sizeOfNewSets = length + 1
            val updatedSetsOfSize = acc.getOrElse(sizeOfNewSets, List.empty) ++ setsWithNewPosition
            acc += (sizeOfNewSets -> updatedSetsOfSize)
          }
        }
        acc += 1 -> (acc.getOrElse(1, List.empty) :+ position)

        potentialBoardEvolutionsHelper(acc, tail, maxLength)
    }

  private def filterToRewardingEvolutions(
    board: EncodedPositions,
    availablePrizeLines: Set[PrizeLine],
    evolutions: List[EncodedPositions]
  ): List[(EncodedPositions, Set[PrizeLine])] =
    evolutions
      .collect(Function.unlift { evolution =>
        val newBoard = evolution + board
        val newPrizeLines = availablePrizeLines.filter(hasHit(newBoard, _))
        Option.when(newPrizeLines.nonEmpty)((evolution, newPrizeLines))
      })
      .sortBy { case (_, prizeLines) => -prizeLines.size }
}
