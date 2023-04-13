package ddm

object Reel {
  sealed trait Value
  object Value {
    case object SuperJoker extends Value
    case object Joker extends Value
    final case class Number(raw: Int) extends Value
  }
}

final case class Reel(values: List[Reel.Value])
