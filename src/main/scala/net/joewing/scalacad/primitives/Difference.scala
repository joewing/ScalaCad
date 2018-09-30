package net.joewing.scalacad.primitives

import net.joewing.scalacad._

case class Difference[D <: Dim](base: Primitive[D], minus: Primitive[D]) extends Primitive[D] {
  implicit val dim: D = base.dim
  protected def render: RenderedObject = Invert(Union(Invert(base), minus)).rendered

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    Difference(base.transformed(f), minus.transformed(f))

  override lazy val minBound: Vertex = base.minBound
  override lazy val maxBound: Vertex = base.maxBound
}
