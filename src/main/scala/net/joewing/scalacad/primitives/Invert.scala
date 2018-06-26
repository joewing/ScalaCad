package net.joewing.scalacad.primitives

import net.joewing.scalacad.BSPTree

case class Invert[D <: Dim](obj: Primitive[D]) extends Primitive[D] {
  def render: BSPTree = obj.render.inverted
}

