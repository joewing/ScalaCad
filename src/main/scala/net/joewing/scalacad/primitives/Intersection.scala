package net.joewing.scalacad.primitives

import net.joewing.scalacad.BSPTree

case class Intersection[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: BSPTree = Invert(Union(Invert(a), Invert(b))).render
}

