package net.joewing.scalacad.primitives

import net.joewing.scalacad.{BSPTree, Polygon}

case class Difference[D <: Dim](base: Primitive[D], minus: Primitive[D]) extends Primitive[D] {
  def render: Seq[Polygon] = Invert(Union(Invert(base), minus)).render
}

