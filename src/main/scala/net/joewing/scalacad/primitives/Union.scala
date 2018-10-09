package net.joewing.scalacad.primitives

import net.joewing.scalacad._

import scala.concurrent.{ExecutionContext, Future}

final case class Union[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  val dim: D = a.dim

  private def fastUnion(
    ar: RenderedObject,
    br: RenderedObject
  )(implicit ec: ExecutionContext): Future[RenderedObject] = {
    val mina = a.minBound
    val maxa = a.maxBound
    val minb = b.minBound
    val maxb = b.maxBound
    if (
      maxa.x < minb.x || maxa.y < minb.y || maxa.z < minb.z ||
        mina.x > maxb.x || mina.y > maxb.y || mina.z > maxb.z
    ) {
      // No overlap, so just merge.
      ar.merge(br)
    } else {
      ar.union(br)
    }
  }

  protected def render(implicit ec: ExecutionContext): Future[RenderedObject] = {
    val leftFuture = a.renderedFuture
    val rightFuture = b.renderedFuture
    for {
      left <- leftFuture
      right <- rightFuture
      result <- fastUnion(left, right)
    } yield result
  }

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] = Union(a.transformed(f), b.transformed(f))

  lazy val minBound: Vertex = a.minBound.min(b.minBound)
  lazy val maxBound: Vertex = a.maxBound.max(b.maxBound)
}

