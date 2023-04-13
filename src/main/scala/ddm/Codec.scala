package ddm

final class Codec(boardSize: Int) {
  private val maxBinaryLength = boardSize * boardSize

  private val emptyBoardString: List[Char] =
    List.fill(maxBinaryLength)('0')

  val emptyBoard: EncodedPositions =
    encode(List.empty)

  def encode(position: Position): EncodedPositions =
    encode(List(position))

  def encode(positions: Iterable[Position]): EncodedPositions =
    EncodedPositions(
      java.lang.Integer.parseInt(
        toBinary(positions).mkString,
        2
      )
    )

  private def toBinary(positions: Iterable[Position]): List[Char] =
    positions.foldLeft(emptyBoardString)((acc, position) =>
      acc.updated(toBinaryIndex(position), '1')
    )

  private def toBinaryIndex(position: Position): Int =
    ((position.row - 1) * boardSize) + position.column - 1

  def decode(positions: EncodedPositions): List[Position] = {
    val binary = positions.raw.toBinaryString
    val indexOffset = maxBinaryLength - binary.length + 1

    binary
      .zipWithIndex
      .collect { case (bit, baseIndex) if bit == '1' => baseIndex + indexOffset }
      .map { index =>
        val column = ((index - 1) % boardSize) + 1
        val row = ((index - 1) / boardSize) + 1

        Position(column, row)
      }
      .toList
  }
}
