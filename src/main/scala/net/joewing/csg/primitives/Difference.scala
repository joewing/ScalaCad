package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Difference[D <: Dim](base: Primitive[D], minus: Primitive[D]) extends Primitive[D] {
  def render: BSPTree = {
    val a = base.render.inverted.clip(minus.render)
    val b = minus.render.clip(a).inverted.clip(a).inverted
    a.merge(b).inverted
  }
}

