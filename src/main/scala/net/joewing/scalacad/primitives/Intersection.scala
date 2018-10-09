package net.joewing.scalacad.primitives

import net.joewing.scalacad.{RenderedObject, Vertex}

import scala.concurrent.{ExecutionContext, Future}

final case class Intersection[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  val dim: D = a.dim

  protected def render(implicit ec: ExecutionContext): Future[RenderedObject] = {
    val leftFuture = a.renderedFuture
    val rightFuture = b.renderedFuture
    for {
      left <- leftFuture
      right <- rightFuture
      result <- left.intersect(right)
    } yield result
  }

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    Intersection(a.transformed(f), b.transformed(f))

  lazy val minBound: Vertex = a.minBound.max(b.minBound)
  lazy val maxBound: Vertex = a.maxBound.min(b.maxBound)
}

