package net.joewing.scalacad.primitives

import net.joewing.scalacad.BSPTree

case class Union[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: BSPTree = {
    val left = a.render
    val right = b.render
    val leftClipped = left.clip(right)
    val rightClipped = right.clip(leftClipped).inverted.clip(leftClipped).inverted
    leftClipped.merge(rightClipped)
  }
}

