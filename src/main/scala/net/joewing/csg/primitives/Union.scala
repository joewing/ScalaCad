package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Union[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: BSPTree = {
    val left = a.render
    val right = b.render
    val leftClipped = left.clip(right)
    val rightClipped = right.clip(left).inverted.clip(leftClipped).inverted
    leftClipped.merge(rightClipped)
  }
}

