package net.joewing.scalacad.primitives

import net.joewing.scalacad._

final case class Union[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  implicit val dim: D = a.dim
  lazy val render: RenderedObject = {
    val left = a.render.tree
    val right = b.render.tree
    val leftClipped = left.clip(right)
    val rightClipped = right.clip(leftClipped).inverted.clip(leftClipped).inverted
    BSPTreeRenderedObject(dim, leftClipped.merge(rightClipped))
  }
}

