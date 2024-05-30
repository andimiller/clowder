package net.andimiller.clowder.clustering

import munit.ScalaCheckSuite
import net.andimiller.clowder.Distance
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}

class SimpleClustererSpec extends munit.FunSuite with ScalaCheckSuite {
  test("should cluster some integers by distance") {
    import Distance.Examples.numericDifference

    val clusterer = new SimpleClusterer[Int](2)
    assertEquals(
      clusterer(
        Vector(0, 1, 2, 4, 5, 6, 7, 8, 9)
      ),
      Vector(Vector(0, 1, 2), Vector(4, 5, 6, 7, 8), Vector(9))
    )
  }

  test("should cluster sets based on jaccard") {
    import Distance.Examples.setJaccard

    val clusterer = new SimpleClusterer[Set[String]](threshold = 0.5)
    assertEquals(
      clusterer(
        Vector(
          Set("a"),
          Set("a", "b"),
          Set("c"),
          Set("hello"),
          Set("world"),
          Set("hello", "world")
        )
      ),
      Vector(
        Vector(Set("a"), Set("a", "b")),
        Vector(Set("c")),
        Vector(Set("hello"), Set("hello", "world"), Set("world"))
      )
    )
  }
  case class PositiveInt(i: Int)

  property("for any list of longs, groups should have a max distance of threshold from the centre") {
    implicit val intsGen: Arbitrary[Vector[Long]]     = Arbitrary(Gen.nonEmptyListOf(Gen.chooseNum(0L, 100L)).map(_.toVector))
    implicit val thresholdGen: Arbitrary[PositiveInt] = Arbitrary(Gen.chooseNum(1, 100).filter(_ > 0).map(PositiveInt))
    forAll { (is: Vector[Long], threshold: PositiveInt) =>
      import Distance.Examples.numericDifference
      val clusterer = new SimpleClusterer[Long](threshold.i.toDouble)
      clusterer(is).forall { g =>
        val d           = numericDifference[Long]
        val centre      = d.centreOption(g).get
        val maxDistance = g.view.map(d.distance(_, centre)).max
        maxDistance <= threshold.i.toDouble
      }
    }
  }
}
