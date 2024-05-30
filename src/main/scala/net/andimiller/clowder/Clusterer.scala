package net.andimiller.clowder

trait Clusterer[T] {
  def apply(inputs: Vector[T]): Vector[Vector[T]]
}