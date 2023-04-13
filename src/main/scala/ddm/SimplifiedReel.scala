package ddm

import ddm.SimplifiedReel.Value

object SimplifiedReel {
  sealed trait Value
  object Value {
    case object Miss extends Value
    final case class Hit(position: EncodedPositions) extends Value
    case object Joker extends Value
    case object SuperJoker extends Value
  }
}

final case class SimplifiedReel(
  superJokerProbability: Double,
  jokerProbability: Double,
  positionProbabilities: Map[EncodedPositions, Double],
  positions: EncodedPositions
) {
  val missProbability: Double =
    1 - superJokerProbability - jokerProbability - positionProbabilities.values.sum

  def probabilityOf(value: SimplifiedReel.Value): Double =
    value match {
      case Value.Miss => missProbability
      case Value.Hit(position) => positionProbabilities.getOrElse(position, 0)
      case Value.Joker => jokerProbability
      case Value.SuperJoker => superJokerProbability
    }
}
