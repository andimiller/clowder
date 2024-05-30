package net.andimiller.clowder.clustering

import net.andimiller.clowder.{Clusterer, Distance}

import scala.annotation.tailrec

class SimpleClusterer[T](threshold: Double)(implicit T: Distance[T]) extends Clusterer[T] {
  override def apply(inputs: Vector[T]): Vector[Vector[T]] = {
    @tailrec
    def cluster(ungrouped: Vector[T], groups: Vector[Vector[T]]): Vector[Vector[T]] = ungrouped match {
      case nextCentre +: rest =>
        @tailrec
        def expand(c: T, group: Vector[T], others: Vector[T]): (Vector[T], Vector[T]) = {
          val (added, notAdded) = others.partition(T.distance(c, _) <= threshold)
          val newGroup          = group ++ added
          val pivot             = T.centreOption(newGroup).get // this is safe because we know this group is nonEmpty
          if (pivot == c) (newGroup, notAdded) else expand(pivot, newGroup, notAdded)
        }

        val (group, remaining) = expand(nextCentre, Vector(nextCentre), rest)
        cluster(remaining, groups.appended(group))
      case Vector()           =>
        groups
    }
    cluster(inputs, Vector.empty)
  }
}
