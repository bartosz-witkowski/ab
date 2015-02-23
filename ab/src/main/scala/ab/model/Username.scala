package ab.model

import scodec._
import scalaz._, Scalaz._

case class Username(chars: NonEmptyList[AlphaNum])

object Username {
  def apply(string: String): ValidationNel[String, Username] = {
    val xs = string.toList.map { c =>
      AlphaNum.fromChar(c).cata({ alphaNum =>
        alphaNum.successNel
      }, { 
        s"'${c}' is not alpha-numeric".failureNel
      })
    }

    xs.sequenceU.fold({ failures =>
      failures.failure
    }, {
      case a :: as =>
        Username(NonEmptyList(a, as: _*)).successNel

      case other =>
        "Id cannot be empty".failureNel
    })
  }
    

  implicit val encoder: Encoder[Username] = new Encoder[Username] {
    override val sizeBound = SizeBound.unknown

    override def encode(u: Username) =
      Encoder.encodeSeq(Encoder[AlphaNum])(u.chars.list)
  }
}
