package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Invert[D <: Dim](obj: Primitive[D]) extends Primitive[D] {
  def render: BSPTree = obj.render.inverted
}

