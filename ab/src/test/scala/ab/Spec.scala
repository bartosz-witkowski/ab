package ab

import org.specs2.matcher._
import org.specs2.mutable.FragmentsBuilder
import org.specs2.Specification
import org.specs2.main.{ArgumentsShortcuts, ArgumentsArgs}

trait Spec extends Specification with FragmentsBuilder with ScalaCheckMatchers with ArgumentsArgs 
  with ArgumentsShortcuts {

  addArguments(fullStackTrace)

  override def is = fragments
}
