package net.joewing.scalacad.primitives

import net.joewing.scalacad.BSPTree

case class Difference[D <: Dim](base: Primitive[D], minus: Primitive[D]) extends Primitive[D] {
  def render: BSPTree = Invert(Union(Invert(base), minus)).render
}

