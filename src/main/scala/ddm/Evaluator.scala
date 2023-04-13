package ddm

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

final class Evaluator(boardSize: Int, board: Board, reels: List[Reel]) {
  private val codec = new Codec(boardSize)
  private val boardEvaluator = new BoardEvaluator(boardSize, codec, PrizeLine.forBoardSize(boardSize, codec))

  def calculateSpinValue(): Double =
    measureRunTime("fullCalculation") {
      val evolutions = measureRunTime("findEvolutions")(boardEvaluator.findRewardingEvolutions(board))
      val meaningfulPositions = evolutions.raw.values.flatten.foldLeft(codec.emptyBoard) { case (acc, (p, _)) => acc + p }

      val reelEvaluator = new ReelEvaluator(meaningfulPositions, board, codec)
      val spinEvaluator = new SpinEvaluator(codec)

      val simplifiedReels = measureRunTime("simplifyReels")(reels.map(reelEvaluator.simplify))
      measureRunTime("evaluateSpins")(spinEvaluator.calculateSpinValue(simplifiedReels, evolutions))
    }

  private def measureRunTime[T](calcName: String)(f: => T): T = {
    val startTime = System.nanoTime()
    val result = f
    val endTime = System.nanoTime()
    println(s"Finished $calcName in ${FiniteDuration(endTime - startTime, TimeUnit.NANOSECONDS).toMillis}ms")
    result
  }
}
