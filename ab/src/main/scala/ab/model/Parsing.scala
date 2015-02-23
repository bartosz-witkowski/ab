package ab.model

import atto._, Atto._
import scalaz._, Scalaz.{char => _, _}

object Parsing {
  sealed abstract class Error
  object Error {
    case class SyntaxError(message: String) extends Error
    case class InvalidWeightInGroup(nfe: NumberFormatException) extends Error
    case object TooFewGroups extends Error

    def syntax(message: String): Error =
      SyntaxError(message)

    def tooFewGroups: Error = TooFewGroups

    def apply(nfe: NumberFormatException): Error = 
      InvalidWeightInGroup(nfe)

  }

  val whitespace = takeWhile(c => c.isWhitespace)

  val groupDescription: Parser[NumberFormatException \/ GroupDescription] = for {
    _      <- whitespace
    _      <- char('(')
    _      <- whitespace
    _      <- char('"')
    group  <- takeWhile(c => c != '"')
    _      <- char('"')
    _      <- whitespace
    _      <- char(',')
    _      <- whitespace
    weight <- takeWhile(c => c != ')')
    _      <- char(')')
  } yield {
    PositiveInt.fromString(weight.trim).map { weight =>
      GroupDescription(Group(group), weight)
    }
  }

  val groups: Parser[ValidationNel[Error, Groups]] = for {
    _    <- whitespace
    _    <- char('(')
    _    <- whitespace
    xs   <- many(groupDescription <~ whitespace <~ string(",") <~ whitespace)
    x    <- groupDescription
    _    <- whitespace
    _    <- char(')')
  } yield {
    val ds: List[ValidationNel[Error, GroupDescription]] = (xs :+ x).map { either =>
      either.leftMap(Error(_)).validation.toValidationNel 
    }

    ds.sequenceU.fold({ errors =>
      errors.failure
    }, { 
      case d1 :: d2 :: ds =>
        Groups.from(d1, d2, ds: _*).successNel
        
      case other =>
        Error.tooFewGroups.failureNel
    })
  }

  def parseGroups(s: String): ValidationNel[Error, Groups] = {
    groups.parseOnly(s).either.fold({ message =>
      Error.syntax(message).failureNel
    }, { validation =>
      validation
    })
  }
}
