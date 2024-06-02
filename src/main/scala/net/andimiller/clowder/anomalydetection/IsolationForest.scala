package net.andimiller.clowder.anomalydetection

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import net.andimiller.clowder.RandomAdaptor.RandomAdaptorSyntax
import net.andimiller.clowder.{AnomalyDetector, RandomAdaptor}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Random

case class FacetExtractor[Input, Facet: Ordering: Numeric: RandomAdaptor: Eq](run: Input => Facet) {
  @inline final def extract(inputs: Vector[Input], depth: Int)(
      results: mutable.Map[Input, Double]
  )(implicit random: util.Random): Vector[Vector[Input]] = {
    val sorted = inputs.map(i => i -> run(i)).sortBy(_._2)
    val max    = sorted.last._2
    val min    = sorted.head._2
    if (min == max) {
      val compensated = depth + IsolationForest.compensation(inputs.size)
      inputs.foreach { v =>
        results.update(v, compensated)
      }
      Vector()
    } else {
      val splitValue = random.between(min, max)
      val splitIndex = sorted.indexWhere(i => i._2 > splitValue)
      val (lte, gt)  = (sorted.map(_._1).slice(0, splitIndex), sorted.map(_._1).slice(splitIndex, sorted.size))
      Vector(lte, gt)
    }
  }
}

object IsolationForest {
  @inline private final val EULERS_CONSTANT: Double = 0.5772156649d

  @inline private final def h(input: Double): Double =
    Math.log(input) + EULERS_CONSTANT

  @inline final def compensation(size: Int): Double = size match {
    case 0 | 1 => 0
    case 2     => 1
    case i     => (2 * h(i - 1)) - ((2 * (i - 1)) / i)
  }
}

class IsolationForest[Input](
    extractors: NonEmptyList[FacetExtractor[Input, _]],
    samples: Int = 256,
    stdDevs: Int = 1,
    maxDepth: Int = 5
)(implicit
    random: Random
) extends AnomalyDetector[Input] {

  @inline private final def pickExtractor(): FacetExtractor[Input, _] = extractors.toList.apply(random.between(0, extractors.size))

  @tailrec
  private def go(inputs: Vector[Vector[Input]], depth: Int)(
      results: mutable.Map[Input, Double]
  ): mutable.Map[Input, Double] = {
    val nextInputs = inputs.flatMap {
      case Vector()                    =>
        Vector()
      case Vector(value)               =>
        results.update(value, depth)
        Vector()
      case values if maxDepth <= depth => // we short circuit at max depth
        val compensated = depth + IsolationForest.compensation(values.size)
        values.foreach { v =>
          results.update(v, compensated)
        }
        Vector()
      case inputs                      => // if there are multiple items we should break it down
        pickExtractor().extract(inputs, depth)(results)
    }
    if (nextInputs.nonEmpty) {
      go(nextInputs, depth + 1)(results)
    } else {
      results
    }
  }

  override def findOutliers(input: Vector[Input]): Vector[Input] = {
    val acc = mutable.Map[Input, Double]()
    for (_ <- 0 to samples)
      go(Vector(input), 0)(mutable.Map.empty).foreach { case (k, v) =>
        acc.updateWith(k)(_.fold(v)(_ + v).some)
      }
    val averageDepths = acc.view.mapValues(_ / samples).toMap
    val meanDepth         = averageDepths.view.values.sum / averageDepths.size
    val deviations        = averageDepths.view.values.map(depth => Math.pow(depth - meanDepth, 2)).sum
    val standardDeviation = Math.sqrt(deviations / averageDepths.size)
    val lowerLimit        = meanDepth - (stdDevs * standardDeviation) // we only care about outliers on the low side
    averageDepths.view.filter { case (_, v) => (v < lowerLimit) }.keys.toVector
  }
}
