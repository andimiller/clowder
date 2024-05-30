package net.andimiller.clowder.anomalydetection

import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

import scala.util.Random

class IsolationForestsSpec extends munit.FunSuite with ScalaCheckSuite {

  test("Should find anomaly in a custom list") {
    implicit val r = new Random(1234)
    val alg        = new IsolationForests[Int](stdDevs = 1)
    assertEquals(
      alg.findOutliers(Vector(1, 2, 3, 4, 5, 11)),
      Vector(11)
    )
  }

  property("should run with many longs") {
    implicit val r                            = new Random(1234)
    val alg                                   = new IsolationForests[Long](samples = 32, stdDevs = 1)
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
