package ddm

object PrizeLine {
  def forBoardSize(size: Int, codec: Codec): Set[PrizeLine] = {
    val rows = (1 to size).map(row =>
      PrizeLine(codec.encode(
        (1 to size).map(column => Position(column, row))
      ))
    )

    val columns = (1 to size).map(column =>
      PrizeLine(codec.encode(
        (1 to size).map(row => Position(column, row))
      ))
    )

    val firstDiagonal = PrizeLine(codec.encode(
      (1 to size).map(i => Position(i, i))
    ))

    val secondDiagonal = PrizeLine(codec.encode(
      (1 to size).map(i => Position(size - i + 1, i)).toSet
    ))

    rows.toSet ++ columns + firstDiagonal + secondDiagonal
  }
}

final case class PrizeLine(positions: EncodedPositions)
