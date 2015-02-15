package ab.model

import scalaz._

case class Username(chars: NonEmptyList[AlphaNum])
