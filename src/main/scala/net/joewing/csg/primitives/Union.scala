package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Union[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: BSPTree = {
    val a1 = a.render
    val b1 = b.render
    val a2 = a1.clip(b1)
    val b2 = b1.clip(a2).inverted.clip(a2).inverted
    a2.merge(b2)
  }
}

