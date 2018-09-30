package net.joewing.scalacad.primitives

import net.joewing.scalacad.{RenderedObject, Vertex}

case class Invert[D <: Dim](obj: Primitive[D]) extends Primitive[D] {
  implicit val dim: D = obj.dim
  protected def render: RenderedObject = obj.rendered.invert

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] = Invert(obj.transformed(f))

  override lazy val minBound: Vertex = obj.maxBound
  override lazy val maxBound: Vertex = obj.minBound
}
