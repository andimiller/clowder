package net.andimiller.clowder.anomalydetection

import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

import scala.util.Random

class IsolationForestSimpleSpec extends munit.FunSuite with ScalaCheckSuite {

  test("Should find anomaly in a custom list") {
    implicit val r = new Random(1234)
    val alg        = new IsolationForestSimple[Int](stdDevs = 1)
    assertEquals(
      alg.findOutliers(Vector(1, 2, 3, 4, 5, 11)),
      Vector(11)
    )
  }

  property("should find an extra value added into a range") {
    implicit val values: Arbitrary[Vector[Int]] = Arbitrary {
      val value = Gen.choose(0, 1000)
      for {
        h0 <- value
        h1 <- value
        h2 <- value
      } yield Vector(h0, h1, h2)
    }
    implicit val extraValue: Arbitrary[Int]     = Arbitrary(Gen.choose(2500, 3000))
    forAll { (ints: Vector[Int], extra: Int) =>
      implicit val r = new Random(1234)
      val alg        = new IsolationForestSimple[Int](samples = 256, stdDevs = 1)
      alg.findOutliers(
        r.shuffle(ints.appended(extra))
      ) == Vector(extra)
    }
  }

  test("Should find anomaly in a custom list with a max depth") {
    implicit val r = new Random(1234)
    val alg        = new IsolationForestSimple[Int](stdDevs = 1, maxDepth = Some(2))
    assertEquals(
      alg.findOutliers(Vector(1, 2, 3, 4, 5, 11)),
      Vector(11)
    )
  }

  property("should run with many longs") {
    implicit val r                            = new Random(1234)
    val alg                                   = new IsolationForestSimple[Long](samples = 32, stdDevs = 1, maxDepth = Some(2))
    implicit val gen: Arbitrary[Vector[Long]] = Arbitrary(
      Gen.choose(1, 1000).flatMap { size =>
        Gen.buildableOfN[Vector[Long], Long](size, Gen.long)
      }
    )
    forAll { longs: Vector[Long] =>
      alg.findOutliers(longs).size <= longs.size
    }
  }
}
