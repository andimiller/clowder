package net.andimiller.clowder.anomalydetection

import cats.data.NonEmptyList
import munit.ScalaCheckSuite
import net.andimiller.clowder.anomalydetection
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}

import scala.util.Random

class IsolationForestSpec extends munit.FunSuite with ScalaCheckSuite {

  test("Should find anomaly in a custom list with only one facet") {
    implicit val r = new Random(1234)
    val alg        = new IsolationForest[Int](extractors = NonEmptyList.of(FacetExtractor(identity)), stdDevs = 1)
    assertEquals(
      alg.findOutliers(Vector(1, 2, 3, 4, 5, 11)),
      Vector(11)
    )
  }

  case class Input(name: String, x: Int, y: Int)

  test("Should find anomaly in a custom list with two facets") {
    implicit val r = new Random(1234)
    val alg        = new IsolationForest[Input](extractors = NonEmptyList.of(FacetExtractor(_.x), FacetExtractor(_.y)), stdDevs = 1)
    assertEquals(
      alg.findOutliers(
        Vector(
          Input("a", 1, 1),
          Input("b", 2, 2),
          Input("c", 3, 3),
          Input("d", 4, 4),
          Input("e", 5, 4),
          Input("f", 0, 5),
          Input("g", 1, 20)
        )
      ),
      Vector(Input("g", 1, 20))
    )
  }

  test("Should find anomaly in a custom list with three facets") {
    implicit val r = new Random(1234)
    val alg        = new IsolationForest[Input](
      extractors = NonEmptyList.of(
        FacetExtractor(_.x),
        FacetExtractor(_.y),
        FacetExtractor(_.name.hashCode)
      ),
      stdDevs = 1
    )
    assertEquals(
      alg.findOutliers(
        Vector(
          Input("a", 1, 1),
          Input("b", 2, 2),
          Input("whoa very long string", 3, 3),
          Input("d", 4, 4),
          Input("e", 5, 4),
          Input("f", 0, 5),
          Input("g", 1, 20)
        )
      ),
      Vector(
        Input("g", 1, 20),
        Input("whoa very long string", 3, 3)
      )
    )
  }

  property("should run with many inputs") {
    implicit val r                             = new Random(1234)
    val alg                                    = new IsolationForest[Input](
      extractors = NonEmptyList.of(
        FacetExtractor(_.x),
        FacetExtractor(_.y),
        FacetExtractor(_.name.hashCode)
      ),
      samples = 32,
      stdDevs = 1,
      maxDepth = 3
    )
    implicit val gen: Arbitrary[Vector[Input]] = Arbitrary(
      Gen.buildableOf[Vector[Input], Input](
        for {
          name <- Gen.alphaStr
          x    <- Gen.choose(0, 100)
          y    <- Gen.choose(0, 100)
        } yield Input(name, x, y)
      )
    )
    forAll { inputs: Vector[Input] =>
      alg.findOutliers(inputs).size <= inputs.size
    }
  }

  property("should run with many inputs with facets that output many types") {
    case class Input2(name: String, x: Int, y: Long, z: Double)
    implicit val r                              = new Random(1234)
    val alg                                     = new IsolationForest[Input2](
      extractors = NonEmptyList.of(
        FacetExtractor(_.x),
        FacetExtractor(_.y),
        FacetExtractor(_.name.hashCode),
        FacetExtractor(_.z)
      ),
      samples = 32,
      stdDevs = 1,
      maxDepth = 3
    )
    implicit val gen: Arbitrary[Vector[Input2]] = Arbitrary(
      Gen.buildableOf[Vector[Input2], Input2](
        for {
          name <- Gen.alphaStr
          x    <- Gen.choose(0, 100)
          y    <- Gen.choose(0L, 100L)
          z    <- Gen.choose(0.0, 1.0)
        } yield Input2(name, x, y, z)
      )
    )
    forAll { inputs: Vector[Input2] =>
      alg.findOutliers(inputs).size <= inputs.size
    }
  }

}
