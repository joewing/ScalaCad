package net.joewing.scalacad.primitives

import net.joewing.scalacad._

import scala.concurrent.{ExecutionContext, Future}

final case class Difference[D <: Dim](base: Primitive[D], minus: Primitive[D]) extends Primitive[D] {
  val dim: D = base.dim

  protected def render(implicit ec: ExecutionContext): Future[RenderedObject] = {
    val leftFuture = base.renderedFuture
    val rightFuture = minus.renderedFuture
    for {
      left <- leftFuture
      right <- rightFuture
      result <- left.minus(right)
    } yield result
  }

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    Difference(base.transformed(f), minus.transformed(f))

  lazy val minBound: Vertex = base.minBound
  lazy val maxBound: Vertex = base.maxBound
}
