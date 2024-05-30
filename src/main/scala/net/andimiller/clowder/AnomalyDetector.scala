package net.andimiller.clowder

trait AnomalyDetector[T] {
  def findOutliers(input: Vector[T]): Vector[T]
}
