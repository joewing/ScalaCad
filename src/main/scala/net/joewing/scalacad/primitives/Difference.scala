package net.joewing.scalacad.primitives

import net.joewing.scalacad._

final case class Difference[D <: Dim](base: Primitive[D], minus: Primitive[D]) extends Primitive[D] {
  val dim: D = base.dim

  protected def render: RenderedObject = base.rendered.minus(minus.rendered)

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    Difference(base.transformed(f), minus.transformed(f))

  lazy val minBound: Vertex = base.minBound
  lazy val maxBound: Vertex = base.maxBound
}
