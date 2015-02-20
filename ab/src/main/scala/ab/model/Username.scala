package ab.model

import scodec._
import scalaz._

case class Username(chars: NonEmptyList[AlphaNum])

object Username {
  implicit val encoder: Encoder[Username] = new Encoder[Username] {
    override val sizeBound = SizeBound.unknown

    override def encode(u: Username) =
      Encoder.encodeSeq(Encoder[AlphaNum])(u.chars.list)
  }
}
