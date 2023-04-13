package ddm

final class SpinEvaluator(codec: Codec) {
  def calculateSpinValue(reels: List[SimplifiedReel], evolutions: BoardEvaluator.Result): Double =
    enumerateSpins(reels)
      .map(calculateValue(_, evolutions))
      .sum

  private def enumerateSpins(reels: List[SimplifiedReel]): List[List[(SimplifiedReel.Value, SimplifiedReel)]] = {
    val buckets = reels.map { reel =>
      val maybeMiss = Option.when(reel.missProbability > 0)(SimplifiedReel.Value.Miss)
      val maybeJoker = Option.when(reel.jokerProbability > 0)(SimplifiedReel.Value.Joker)
      val maybeSuperJoker = Option.when(reel.superJokerProbability > 0)(SimplifiedReel.Value.SuperJoker)
      val positions = reel.positionProbabilities.keys.map(SimplifiedReel.Value.Hit).toList

      reel -> (positions ++ maybeMiss ++ maybeJoker ++ maybeSuperJoker)
    }

    buckets.foldLeft(List(List.empty[(SimplifiedReel.Value, SimplifiedReel)])) { case (spins, (reel, values)) =>
      for {
        spin <- spins
        value <- values
      } yield spin :+ (value, reel)
    }
  }

  // Explanation:
  // The board evaluator returns results grouped by the number of non-miss positions required
  // to achieve the result. If you only need `Position(1, 1)` to score a prize line, the
  // board evaluator will return all combinations of hits that result in `Position(1, 1)`
  // being occupied. Within the returned groups, position sets that result in the most
  // prize lines are returned first.
  //
  // With this in mind, we know for each spin how many total hits we have, so we know which
  // group to inspect. And we just need to find the first position set compatible with our
  // combination of hits and jokers, since we know that will be the highest scoring result
  // possible.
  //
  // We know that our resulting position set must contain any non-joker hits. For normal
  // jokers, each joker can correspond to any position on the reel. If we assume that numbers
  // on the board are unique, and that the same number does not appear on more than one reel,
  // then it follows that we can check the resulting position set for a simple overlap with
  // all of the reels which span regular jokers.
  //
  // Super jokers turn out to be trivial for us. Any position sets left after the checks for
  // non-joker hits and normal jokers must necessarily be compatible with our remaining super
  // jokers, because the super jokers can be placed anywhere.
  private def calculateValue(
    spin: List[(SimplifiedReel.Value, SimplifiedReel)],
    evolutions: BoardEvaluator.Result
  ): Double = {
    val totalHits = spin.count {
      case (SimplifiedReel.Value.Miss, _) => false
      case _ => true
    }

    val prizeLines =
      evolutions
        .get(totalHits)
        .find { case (evolution, _) =>
          val positionsThatMustBeContained =
            spin
              .collect { case (SimplifiedReel.Value.Hit(position), _) => position }
              .foldLeft(codec.emptyBoard)(_ + _)

          val positionSetsThatMustOverlap =
            spin.collect { case (SimplifiedReel.Value.Joker, reel) => reel.positions }

          evolution.contains(positionsThatMustBeContained) &&
            positionSetsThatMustOverlap.forall(_.overlaps(evolution))
        }
        .map { case (_, prizeLines) => prizeLines }
        .toSet
        .flatten

    val probabilityOfSpin = spin.map { case (value, reel) => reel.probabilityOf(value) }.product
    prizeLines.size * probabilityOfSpin
  }
}
