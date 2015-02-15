package ab.model

import ab.Spec
import scalaz._, Scalaz._

class GroupsSpec extends Spec {
  "Groups" should {
    val a = Group("A")
    val b = Group("B")
    val c = Group("C")


    val fixedCase = (PositiveInt(1) |@| PositiveInt(2) |@| PositiveInt(3)) { (_1, _2, _3) =>
      Groups.from(
        GroupDescription(a, _1),
        GroupDescription(b, _2),
        GroupDescription(c, _3))
    }.getOrElse(throw new RuntimeException("PositiveInt > 0"))

    "bucketRelationship" in {
      fixedCase.bucketRelationship(0L, 0) must_== Ordering.EQ 
      fixedCase.bucketRelationship(1L, 0) must_== Ordering.GT 

      fixedCase.bucketRelationship(0L, 1) must_== Ordering.LT 
      fixedCase.bucketRelationship(1L, 1) must_== Ordering.EQ 
      fixedCase.bucketRelationship(2L, 1) must_== Ordering.EQ 
      fixedCase.bucketRelationship(3L, 1) must_== Ordering.GT 

      fixedCase.bucketRelationship(2L, 2) must_== Ordering.LT 
      fixedCase.bucketRelationship(3L, 2) must_== Ordering.EQ 
      fixedCase.bucketRelationship(4L, 2) must_== Ordering.EQ 
      fixedCase.bucketRelationship(5L, 2) must_== Ordering.EQ 
      fixedCase.bucketRelationship(6L, 2) must_== Ordering.GT 
    }

    "groupFor" in {
      fixedCase.groupFor(0L) must_== a
      fixedCase.groupFor(1L) must_== b
      fixedCase.groupFor(2L) must_== b
      fixedCase.groupFor(3L) must_== c
      fixedCase.groupFor(4L) must_== c
      fixedCase.groupFor(5L) must_== c
      fixedCase.groupFor(6L) must_== a
    }

    "groupFor modulo" ! prop { (x: Long) =>
      fixedCase.groupFor(x) must_== fixedCase.groupFor(x % 6)
    }

    "groupFor absolute " ! prop { (x: Long) =>
      fixedCase.groupFor(x) must_== fixedCase.groupFor(math.abs(x))
    }
  }
}
