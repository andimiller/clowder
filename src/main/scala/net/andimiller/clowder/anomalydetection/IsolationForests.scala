package net.andimiller.clowder.anomalydetection

import cats.implicits.catsSyntaxOptionId
import net.andimiller.clowder.RandomAdaptor.RandomADaptorSyntax
import net.andimiller.clowder.{AnomalyDetector, RandomAdaptor}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered
import scala.util.Random

class IsolationForests[T: Ordering: Numeric: RandomAdaptor](samples: Int = 256, stdDevs: Int = 1)(implicit random: Random)
    extends AnomalyDetector[T] {

  @tailrec
  private def go(inputs: Vector[Vector[T]], depth: Int)(results: mutable.Map[T, Int]): mutable.Map[T, Int] = {
    val nextInputs = inputs.flatMap {
      case Vector()      =>
        Vector()
      case Vector(value) =>
        results.update(value, depth)
        Vector()
      case sorted        => // if there are multiple items we should break it down
        val max        = sorted.last
        val min        = sorted.head
        val splitValue = random.between(min, max)
        val splitIndex = sorted.indexWhere(_ > splitValue)
        val (lte, gt)  = sorted.splitAt(splitIndex)
        Vector(lte, gt)
    }
    if (nextInputs.nonEmpty) {
      go(nextInputs, depth + 1)(results)
    } else {
      results
    }
  }

  override def findOutliers(input: Vector[T]): Vector[T] = {
    val acc           = mutable.Map[T, Int]()
    val distinctInput = input.distinct.sorted
    for (_ <- 0 to samples)
      go(Vector(distinctInput), 0)(mutable.Map.empty).foreach { case (k, v) =>
        acc.updateWith(k)(_.fold(v)(_ + v).some)
      }
    val averageDepths = acc.view.mapValues(_.toDouble / samples).toMap
    val meanDepth         = averageDepths.view.values.sum / averageDepths.size
    val deviations        = averageDepths.view.values.map(depth => Math.pow(depth - meanDepth, 2)).sum
    val standardDeviation = Math.sqrt(deviations / averageDepths.size)
    val lowerLimit        = meanDepth - (stdDevs * standardDeviation) // we only care about outliers on the low side
    averageDepths.view.filter { case (_, v) => (v < lowerLimit) }.keys.toVector
  }
}
