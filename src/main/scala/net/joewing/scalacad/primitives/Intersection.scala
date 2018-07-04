package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Polygon}

case class Intersection[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: Seq[Polygon] = Invert(Union(Invert(a), Invert(b))).render
}

