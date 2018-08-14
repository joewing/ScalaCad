package net.joewing.scalacad.primitives

import net.joewing.scalacad.RenderedObject

case class Intersection[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  lazy val render: RenderedObject = Invert(Union(Invert(a), Invert(b))).render
}

