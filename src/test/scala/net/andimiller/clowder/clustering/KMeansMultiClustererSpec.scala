package net.andimiller.clowder.clustering

import munit.ScalaCheckSuite
import net.andimiller.clowder.Distance
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}

import scala.util.Random

class KMeansMultiClustererSpec extends munit.FunSuite with ScalaCheckSuite {

  test("should cluster some integer into N groups") {
    import Distance.Examples.numericDifference

    val random    = new Random(123)
    val clusterer = new KMeansMultiClusterer[Int](3, 10, random)
    assertEquals(
      clusterer(
        Vector(0, 1, 2, 4, 5, 6, 7, 8, 9)
      ),
      Vector(Vector(4, 5, 6), Vector(0, 1, 2), Vector(7, 8, 9))
    )
  }

  test("should cluster sets based on jaccard into N groups") {
    import Distance.Examples.setJaccard

    val random    = new Random(123)
    val clusterer = new KMeansMultiClusterer[Set[String]](2, 10, random)
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
      Vector(Vector(Set("a"), Set("a", "b"), Set("c")), Vector(Set("hello"), Set("world"), Set("hello", "world")))
    )
  }

  property("for any distinct list of longs, the right number of groups should be formed") {
    val gen = for {
      groups <- Gen.chooseNum(1, 100).filter(_ > 0)
      input  <- Gen.nonEmptyListOf(Gen.chooseNum(0L, 100L)).map(_.distinct.toVector).filter(_.size >= groups)
    } yield groups -> input

    implicit val arb: Arbitrary[(Int, Vector[Long])] = Arbitrary(gen)

    forAll[(Int, Vector[Long]), Boolean] { (input: (Int, Vector[Long])) =>
      val (centroids: Int, is: Vector[Long]) = input
      import Distance.Examples.numericDifference
      val random                             = new Random(123)
      val clusterer                          = new KMeansMultiClusterer[Long](centroids, 10, random)
      clusterer(is).size == centroids
    }
  }
}
