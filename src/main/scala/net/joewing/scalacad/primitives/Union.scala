package net.joewing.scalacad.primitives

import net.joewing.scalacad._

import scala.concurrent.{ExecutionContext, Future}

final case class Union[D <: Dim](a: Primitive[D], b: Primitive[D], disjoint: Boolean) extends Primitive[D] {
  val dim: D = a.dim

  protected def render(implicit ec: ExecutionContext): Future[RenderedObject] = {
    val leftFuture = a.renderedFuture
    val rightFuture = b.renderedFuture
    for {
      left <- leftFuture
      right <- rightFuture
      result <- if (disjoint || !left.overlaps(right)) Future.successful(left.merge(right)) else left.union(right)
    } yield result
  }

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    Union(a.transformed(f), b.transformed(f), disjoint)

  override def extruded(
    f: Primitive[TwoDimensional] => Primitive[ThreeDimensional]
  ): Primitive[ThreeDimensional] = Union(a.extruded(f), b.extruded(f), disjoint)

  lazy val minBound: Vertex = a.minBound.min(b.minBound)
  lazy val maxBound: Vertex = a.maxBound.max(b.maxBound)
}

object Union {
  def disjoint[D <: Dim](a: Primitive[D], b: Primitive[D]): Union[D] = Union(a, b, disjoint = true)
  def union[D <: Dim](a: Primitive[D], b: Primitive[D]): Union[D] = Union(a, b, disjoint = false)
}

