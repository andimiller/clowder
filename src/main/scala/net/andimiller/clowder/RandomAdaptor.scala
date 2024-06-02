package net.andimiller.clowder

import scala.util.Random

trait RandomAdaptor[O] {
  def between(minInclusive: O, maxExclusive: O)(implicit r: Random): O
}

object RandomAdaptor {
  def apply[O](implicit r: RandomAdaptor[O]): RandomAdaptor[O] = r

  implicit def intAdaptor: RandomAdaptor[Int] = new RandomAdaptor[Int] {
    override def between(minInclusive: Int, maxExclusive: Int)(implicit r: Random): Int = r.between(minInclusive, maxExclusive)
  }

  implicit def longAdaptor: RandomAdaptor[Long] = new RandomAdaptor[Long] {
    override def between(minInclusive: Long, maxExclusive: Long)(implicit r: Random): Long = r.between(minInclusive, maxExclusive)
  }

  implicit def doubleAdaptor: RandomAdaptor[Double] = new RandomAdaptor[Double] {
    override def between(minInclusive: Double, maxExclusive: Double)(implicit r: Random): Double = r.between(minInclusive, maxExclusive)
  }

  implicit class RandomAdaptorSyntax[O](r: Random)(implicit adaptor: RandomAdaptor[O]) {
    def between(minInclusive: O, maxExclusive: O): O = adaptor.between(minInclusive, maxExclusive)(r)
  }
}
