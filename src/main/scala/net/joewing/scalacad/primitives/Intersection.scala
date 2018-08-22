package net.joewing.scalacad.primitives

import net.joewing.scalacad.RenderedObject

case class Intersection[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  implicit val dim: D = a.dim
  protected def render: RenderedObject = Invert(Union(Invert(a), Invert(b))).rendered

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    Intersection(a.transformed(f), b.transformed(f))
}

