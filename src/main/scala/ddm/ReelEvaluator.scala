package ddm

final class ReelEvaluator(meaningfulPositions: EncodedPositions, board: Board, codec: Codec) {
  private val numberToPosition =
    board.cells.map { case (position, cell) =>
      cell.value -> codec.encode(position)
    }

  def simplify(reel: Reel): SimplifiedReel = {
    val uniqueNumbers = reel.values.toSet.collect[Reel.Value.Number] { case n: Reel.Value.Number => n }
    val meaningfulPositionsToNumbers =
      uniqueNumbers.collect(Function.unlift(n =>
        numberToPosition
          .get(n.raw)
          .filter(meaningfulPositions.overlaps)
          .map(_ -> n)
      ))

    SimplifiedReel(
      superJokerProbability = probabilityOf(Reel.Value.SuperJoker, reel),
      jokerProbability = probabilityOf(Reel.Value.Joker, reel),
      positionProbabilities = meaningfulPositionsToNumbers.map { case (position, n) =>
        position -> probabilityOf(n, reel)
      }.toMap,
      positions = meaningfulPositionsToNumbers.foldLeft(codec.emptyBoard) { case (acc, (position, _)) =>
        acc + position
      }
    )
  }

  private def probabilityOf(value: Reel.Value, reel: Reel): Double =
    reel.values.count(_ == value).toDouble / reel.values.size
}
