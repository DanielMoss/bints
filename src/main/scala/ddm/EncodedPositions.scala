package ddm

final case class EncodedPositions(raw: Int) {
  def +(that: EncodedPositions): EncodedPositions =
    EncodedPositions(raw | that.raw)

  def contains(that: EncodedPositions): Boolean =
    (raw & that.raw) == that.raw

  def overlaps(that: EncodedPositions): Boolean =
    (raw & that.raw) > 0
}
