package net.joewing.scalacad.primitives

import net.joewing.scalacad.Surface

case class Intersection[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  lazy val render: Surface = Invert(Union(Invert(a), Invert(b))).render
}

