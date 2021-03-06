package ab.model

import scalaz._, Scalaz._

case class Group(name: String)

case class GroupDescription(group: Group, weight: PositiveInt)

/**
 * Holds the group data in a Vector for efficient binary searching. If we have 3
 * groups:
 *  A: 2
 *  B: 3
 *  C: 4
 *
 * Then the "layout" might look like
 * AABBBCCCC
 * 0-2--5---9
 *  1 34 678
 *
 * This means that if we test if some long belongs to a bucket it must be < then
 * the cumulative for that bucket and > then the one next.
 */
class Groups private (buckets: Vector[Groups.Bucket]) {
  private[this] val sum = buckets.last.cumulative

  def groupFor(l: Long): Group = {
    binSearch(math.abs(l % sum))
  }

  def probabilites: Map[Group, Double] = {
    val sizes: Seq[Long] = for (i <- 0 until buckets.size) yield {
      if (i == 0) {
        buckets(0).cumulative
      } else {
        buckets(i).cumulative - buckets(i - 1).cumulative
      }
    }

    val names = buckets.map(_.group)

    names.zip(sizes).map { case (group, size) =>
      val p = size.toDouble / sum
      (group, p)
    }.toMap
  }

  override def toString = s"""Groups(${buckets.mkString(", ")})"""

  private[this] def binSearch(l: Long): Group = {
    @scala.annotation.tailrec
    def go(low: Int, high: Int): Group = {
      val middle = (low + high) / 2

      val ordering = bucketRelationship(l, middle)

      bucketRelationship(l, middle) match {
        case Ordering.LT => go(low, middle - 1)
        case Ordering.EQ => buckets(middle).group
        case Ordering.GT => go(middle + 1, high)
      }
    }

    go(0, buckets.size - 1)
  }

  private[model] def bucketRelationship(l: Long, n: Int): Ordering = {
    val smallerThenThis = l < buckets(n).cumulative 

    if (smallerThenThis) {
      @inline def noPrevious = n <= 0
      @inline def notInAnyPrevious = l >= buckets(n - 1).cumulative

      if (noPrevious || notInAnyPrevious) {
        Ordering.EQ
      } else {
        Ordering.LT
      }
    } else {
      Ordering.GT
    } 
  }
}

object Groups {
  private case class Bucket(cumulative: Long, group: Group)

  def from(g1: GroupDescription, g2: GroupDescription, gs: GroupDescription*): Groups = {
    val acc = List(Bucket(g1.weight.toLong, g1.group))

    val xs = (g2 :: gs.toList).foldLeft(acc) { case (acc, g) =>
      val cumulative = acc.head.cumulative + g.weight.toLong
      Bucket(cumulative, g.group) :: acc
    }

    new Groups(xs.reverse.toVector)
  }

  import scalaz.concurrent.Task

  sealed abstract class AquisitionError
  case object CannotAquireGroupDescriptionFile extends AquisitionError
  case class ParsingError(error: Parsing.Error) extends AquisitionError

  def fromResource(resourceName: String): Task[ValidationNel[AquisitionError, Groups]] = Task {
    \/.fromTryCatchNonFatal {
      val is = getClass.getResourceAsStream(resourceName)
      scala.io.Source.fromInputStream(is).mkString
    }.fold({ error =>
      CannotAquireGroupDescriptionFile.failureNel
    }, { string =>
      Parsing.parseGroups(string).leftMap { _.map(ParsingError(_)) }
    })
  }
}
