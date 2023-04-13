package ddm

final case class Cell(value: Int, state: Cell.State)

object Cell {
  sealed trait State

  object State {
    case object _Hit extends State
    case object Miss extends State
  }
}