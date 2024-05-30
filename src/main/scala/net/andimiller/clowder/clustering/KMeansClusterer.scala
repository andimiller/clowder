package net.andimiller.clowder.clustering

import net.andimiller.clowder.{Clusterer, Distance}

import scala.annotation.tailrec
import scala.util.Random

class KMeansClusterer[T](centroidCount: Int, random: Random)(implicit T: Distance[T]) extends Clusterer[T] {
  override def apply(inputs: Vector[T]): Vector[Vector[T]] = {
    val initialCentroids = random.shuffle(inputs).take(centroidCount)
    val initialGroups    = inputs.groupBy { t =>
      initialCentroids.minBy(T.distance(t, _))
    }

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

    iterate(initialGroups).values.toVector
  }
}
