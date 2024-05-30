package net.andimiller.clowder.clustering

import net.andimiller.clowder.{Clusterer, Distance}

import scala.annotation.tailrec
import scala.util.Random

// This uses KMeans but runs it with N different seeds keeping track of the best distributed result and returns that
class KMeansMultiClusterer[T](centroidCount: Int, iterations: Int, random: Random)(implicit T: Distance[T]) extends Clusterer[T] {

  private def getVariance(groups: Vector[Vector[T]]): Int = {
    val sizes = groups.map(_.size)
    sizes.max - sizes.min
  }

  override def apply(inputs: Vector[T]): Vector[Vector[T]] = {
    @tailrec
    def iterate(groups: Map[T, Vector[T]]): Map[T, Vector[T]] = {
      val newCentroids = groups.values.map { g =>
        T.centreOption(g).get
      }
      val nextGroups   = inputs.groupBy { t =>
        newCentroids.minBy(T.distance(t, _))
      }
      if (nextGroups == groups) nextGroups else iterate(nextGroups)
    }

    LazyList
      .fill(iterations)(random.shuffle(inputs).take(centroidCount))
      .map { initialCentroids =>
        val initialGroups = inputs.groupBy { t =>
          initialCentroids.minBy(T.distance(t, _))
        }
        iterate(initialGroups).values.toVector
      }
      .minBy(getVariance)
  }
}
