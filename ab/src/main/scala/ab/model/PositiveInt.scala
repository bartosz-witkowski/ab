package ab.model

class PositiveInt private (val underlying: Int) extends AnyVal {
  override def toString = s"PositiveInt(${underlying})"
  def toLong = underlying.toLong
}

object PositiveInt {
  def apply(i: Int): Option[PositiveInt] = 
    if (i > 0) Some(new PositiveInt(i)) else None
}
