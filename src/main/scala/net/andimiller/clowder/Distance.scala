package net.andimiller.clowder

import cats.implicits._
import cats.{Foldable, Functor, Reducible}

trait Distance[T] {
  def distance(a: T, b: T): Double
  def centre[M[_]: Reducible: Functor](ts: M[T]): T              =
    ts.minimumBy(t => ts.foldLeft(0d)((i, t2) => distance(t, t2).max(i)))
  def centreOption[M[_]: Foldable: Functor](ts: M[T]): Option[T] =
    ts.minimumByOption { t =>
      ts.foldLeft(0d)((i, t2) => distance(t, t2).max(i))
    }
}

object Distance {
  def apply[T](implicit d: Distance[T]): Distance[T] = d

  // some examples you can use, import them if you want, they may be correct for you
  object Examples {
    implicit def numericDifference[T](implicit T: Numeric[T]): Distance[T] = { case (a, b) => T.toDouble(T.abs(T.minus(a, b))) }
    implicit def setJaccard[T]: Distance[Set[T]]                           = { case (a, b) =>
      // we do 1- to invert the jaccard because this is about distance, and things that are the same have 0 distance
      1 - a.intersect(b).size.toDouble / a.union(b).size.toDouble
    }
  }
}
