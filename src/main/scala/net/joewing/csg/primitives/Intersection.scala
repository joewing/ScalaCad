package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Intersection[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: BSPTree = {
    val a1 = a.render.inverted
    val b1 = b.render.clip(a1).inverted
    val a2 = a1.clip(b1)
    val b2 = b1.clip(a2)
    a2.merge(b2).inverted
  }
}

