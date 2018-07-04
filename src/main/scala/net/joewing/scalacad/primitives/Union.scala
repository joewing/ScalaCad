package net.joewing.scalacad.primitives

import net.joewing.scalacad.{BSPTree, Polygon}

case class Union[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: Seq[Polygon] = {
    val left = BSPTree(a.render)
    val right = BSPTree(b.render)
    val leftClipped = left.clip(right)
    val rightClipped = right.clip(leftClipped).inverted.clip(leftClipped).inverted
    leftClipped.merge(rightClipped).allPolygons
  }
}

