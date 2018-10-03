package net.joewing.scalacad.primitives

import net.joewing.scalacad.{RenderedObject, Vertex}

final case class Intersection[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  val dim: D = a.dim

  protected def render: RenderedObject = Invert(Union(Invert(a), Invert(b))).rendered

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    Intersection(a.transformed(f), b.transformed(f))

  lazy val minBound: Vertex = a.minBound.max(b.minBound)
  lazy val maxBound: Vertex = a.maxBound.min(b.maxBound)
}

