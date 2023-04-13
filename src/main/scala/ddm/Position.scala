package ddm

object Position {
  def forBoardSize(size: Int): Set[Position] =
    (for {
      column <- 1 to size
      row <- 1 to size
    } yield Position(column, row)).toSet

  implicit val ordering: Ordering[Position] =
    Ordering.by(p => (p.column, p.row))
}

final case class Position(column: Int, row: Int)
