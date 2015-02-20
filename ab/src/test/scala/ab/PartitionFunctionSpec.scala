package ab

import scalaz._, Scalaz._
import ab.model._

import org.scalacheck._
import Arbitrary._

class PartitionFunctionSpec extends Spec {
  val SampleSize = 1 * 1000 * 1000
  val MaxDelta = 0.01

  def generateUsernames(n: Int): Vector[Username] = {
    val usernames0 = AlphaNum.allValues.map { a =>
      Username(NonEmptyList(a))
    }.toStream

    lazy val usernames: Stream[Username] = 
      usernames0 #:::
      (for {
        u1 <- usernames
        u2 <- usernames0
      } yield Username(u1.chars.append(u2.chars)) )

    usernames.take(n).toVector
  }

  val positiveInt: Gen[PositiveInt] = for {
    int    <- Gen.choose(1, 200) 
    pint   = PositiveInt(int)
    if (pint.isDefined)
  } yield pint.get // cannot fail because we check for isDefined

  val GroupNames = ('A' to 'Z') map (_.toString)

  val groups: Gen[Groups] = for {
    count        <- Gen.choose(2, 10)
    groupNames   = GroupNames.take(count)
    weights      <- Gen.listOfN(count, positiveInt)
  } yield {
    val descriptions = (groupNames zip weights).map { case (n, w) =>
      GroupDescription(Group(n), w)
    }.toList

    descriptions match {
      case g1 :: g2 :: gs =>
        Groups.from(g1, g2, gs :_ *) 

      case other => 
        throw new RuntimeException("Shouldn't happen")
    }
  }

  val alphaNum: Gen[AlphaNum] = Gen.oneOf(AlphaNum.allValues)
    
  val username: Gen[Username] = for {
    a  <- alphaNum
    as <- Gen.listOf(alphaNum)
  } yield Username(NonEmptyList(a, as : _*))

  val experiment: Gen[Experiment] = for {
    path     <- arbitrary[String]
    username <- username
  } yield Experiment(path, username)

  val Usernames = generateUsernames(SampleSize)

  /**
   * Checking if the partition function has good statistical properties is
   * beyond the scope of this test. I'm going to trust the authors of 
   * Practical Guide to Controlled Experiments on the Web: Listen to Your
   * Customers not to the HiPPO
   * [www.exp-platform.com/documents/guidecontrolledexperiments.pdf]
   * that the md5 hash function provides a random distribution of data and no
   * correlation between multiple experiments (or "paths" in this case)
   *
   * This is only meant to serve as a "sanity" test in case we've introduced any
   * correlations along the way.
   */
  "PartitionFunction" should {
    def partition(g: Groups): PartitionFunction = 
      PartitionFunction.from(g, HashFunction.md5)

    "Be deterministic" ! Prop.forAll(groups, experiment) { (groups, experiment) =>
      val partitionFunction = partition(groups)
      partitionFunction.partition(experiment) === partitionFunction.partition(experiment)
    }

    "Have a good distribution for a fixed path" ! Prop.forAll(groups) { groups =>
      println("Performing experiments on new data this may take a while")

      def experiment(u: Username): Experiment = 
        Experiment("fixed", u)

      val partitionFunction = partition(groups)

      val gs = Usernames.par.map { u =>
        partitionFunction.partition(experiment(u))
      }

      val empiricalProbability = {
        def distribution(xs: collection.immutable.Seq[Group]): Map[Group, Int] = {
          xs.groupBy(identity).map { case (name, xs) =>
            (name, xs.size)
          }
        }

        val dist = distribution(gs.seq)
        val N = dist.map(_._2).sum

        dist.map { case (group, n) =>
          val p = n.toDouble / N
          (group, p)
        }
      }

      val theoreticalProbabilty = groups.probabilites

      val keys = theoreticalProbabilty.keys.toList

      val test = keys.sortBy(_.name).forall { groupName =>
        val delta = theoreticalProbabilty(groupName) - empiricalProbability(groupName)
        println(s"Δ${groupName.name}: ${delta}")
        math.abs(delta) < MaxDelta
      }

      println("Done\n")

      test
    }.set(minTestsOk = 5)
  }
}
