package net.andimiller.clowder

import scala.util.Random

trait RandomAdaptor[O] {
  def next(implicit r: Random): O
  def betweenInclusive(min: O, max: O)(implicit r: Random): O
  def betweenExclusive(min: O, max: O)(implicit r: Random): O
  def between(minInclusive: O, maxExclusive: O)(implicit r: Random): O
}

object RandomAdaptor {
  def apply[O](implicit r: RandomAdaptor[O]): RandomAdaptor[O] = r

  implicit def intAdaptor: RandomAdaptor[Int] = new RandomAdaptor[Int] {
    override def next(implicit r: Random): Int                                 = r.nextInt
    override def betweenInclusive(min: Int, max: Int)(implicit r: Random): Int = r.between(min, max + 1)
    override def betweenExclusive(min: Int, max: Int)(implicit r: Random): Int = r.between(min + 1, max)

    override def between(minInclusive: Int, maxExclusive: Int)(implicit r: Random): Int = r.between(minInclusive, maxExclusive)
  }

  implicit def longAdaptor: RandomAdaptor[Long] = new RandomAdaptor[Long] {
    override def next(implicit r: Random): Long                                            = r.nextLong
    override def betweenInclusive(min: Long, max: Long)(implicit r: Random): Long          = r.between(min, max + 1)
    override def betweenExclusive(min: Long, max: Long)(implicit r: Random): Long          = r.between(min + 1, max)
    override def between(minInclusive: Long, maxExclusive: Long)(implicit r: Random): Long = r.between(minInclusive, maxExclusive)
  }

  implicit class RandomADaptorSyntax[O](r: Random)(implicit adaptor: RandomAdaptor[O]) {
    def nextAdapted: O                            = adaptor.next(r)
    def betweenInclusive(min: O, max: O)          = adaptor.betweenInclusive(min, max)(r)
    def betweenExclusive(min: O, max: O)          = adaptor.betweenExclusive(min, max)(r)
    def between(minInclusive: O, maxExclusive: O) = adaptor.between(minInclusive, maxExclusive)(r)
  }
}
