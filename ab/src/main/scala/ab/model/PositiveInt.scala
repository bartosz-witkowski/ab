package ab.model

import scalaz._, Scalaz._

class PositiveInt private (val underlying: Int) extends AnyVal {
  override def toString = s"PositiveInt(${underlying})"
  def toLong = underlying.toLong
}

object PositiveInt {
  def fromString(s: String): NumberFormatException \/ PositiveInt = {
    s.parseInt.disjunction.flatMap { int =>
      if (int > 0) {
        (new PositiveInt(int)).right
      } else {
        new NumberFormatException("Cannot parse PositiveInt from " + s).left
      }
    }
  }

  def apply(i: Int): Option[PositiveInt] = 
    if (i > 0) Some(new PositiveInt(i)) else None
}
