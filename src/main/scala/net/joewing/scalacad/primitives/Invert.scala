package net.joewing.scalacad.primitives

import net.joewing.scalacad.{RenderedObject, Vertex}

final case class Invert[D <: Dim](obj: Primitive[D]) extends Primitive[D] {
  val dim: D = obj.dim

  protected def render: RenderedObject = obj.rendered.invert

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] = Invert(obj.transformed(f))

  lazy val minBound: Vertex = obj.minBound
  lazy val maxBound: Vertex = obj.maxBound
}
