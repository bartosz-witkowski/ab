package ab.model

import ab.Spec
import scalaz._, Scalaz._
import atto._, Atto._

class ParsingSpec extends Spec {
  import Parsing._

  def parses[A](p: ParseResult[A]): Boolean = {
    p.option.isDefined
  }

  "Parsing GroupDescription" should {
    def parses_\/[A, B](p: ParseResult[A \/ B]): Boolean = {
      p.option.fold {
        false
      } { either =>
        either.fold(_ => false, _ => true)
      }
    }

    "parse trivial data" in {
      parses_\/(groupDescription.parseOnly("""("Grupa A", 3)"""))
    }
    "parse data with spaces 0" in {
      parses_\/(groupDescription.parseOnly(""" ( "Grupa A", 3)"""))
    }
    "parse data with spaces 1" in {
      parses_\/(groupDescription.parseOnly(""" ( "Grupa A", 3)"""))
    }
    "parse data with spaces 2" in {
      parses_\/(groupDescription.parseOnly(""" ( "Grupa A" , 3)""")) 
    }
    "parse data with spaces 3" in {
      parses_\/(groupDescription.parseOnly(""" ( "Grupa A" , 3 )"""))
    }
    "parse data with spaces 4" in {
      parses_\/(groupDescription.parseOnly(""" ( "Grupa A" , 3 ) """))
    }
  }

  "Parsing groups" should {
    def parsesValidation[A, B](p: ParseResult[Validation[A, B]]): Boolean = {
      p.either.fold({ string =>
        println(string)
        false
      }, { validation =>
        validation.fold({ failure => 
          println(failure) 
          false
        },  _ => true)
      })
    }

    "parses trivial data" in {
      parsesValidation(
        groups.parseOnly("""(("grupa A", 2), ("grupa B", 3), ("grupa C", 5))""")
      )
    }

    "parses data with spaces 1" in {
      parsesValidation(
        groups.parseOnly(""" (("grupa A", 2), ("grupa B", 3), ("grupa C", 5))""")
      )
    }

    "parses data with spaces 1" in {
      parsesValidation(
        groups.parseOnly(""" ( ("grupa A", 2), ("grupa B", 3), ("grupa C", 5))""")
      )
    }

    "parses data with spaces 2" in {
      parsesValidation(
        groups.parseOnly(""" ( ("grupa A", 2) , ("grupa B", 3), ("grupa C", 5))""")
      )
    }

    "parses data with spaces 3" in {
      parsesValidation(
        groups.parseOnly(""" ( ("grupa A", 2) , ("grupa B", 3) , ("grupa C", 5))""")
      )
    }

    "parses data with spaces 4" in {
      parsesValidation(
        groups.parseOnly(""" ( ("grupa A", 2) , ("grupa B", 3) , ("grupa C", 5) )""")
      )
    }

    "parses data with spaces 5" in {
      parsesValidation(
        groups.parseOnly(""" ( ("grupa A", 2) , ("grupa B", 3) , ("grupa C", 5) ) """)
      )
    }
  }
}
