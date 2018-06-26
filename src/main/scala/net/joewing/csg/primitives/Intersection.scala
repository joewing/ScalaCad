package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Intersection[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: BSPTree = Invert(Union(Invert(a), Invert(b))).render
}

