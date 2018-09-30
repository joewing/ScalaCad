package net.joewing.scalacad.primitives

import net.joewing.scalacad._

final case class Union[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  implicit val dim: D = a.dim
  protected def render: RenderedObject = {
    val left = a.rendered.tree
    val right = b.rendered.tree
    val leftClipped = left.clip(right)
    val rightClipped = right.clip(leftClipped).inverted.clip(leftClipped).inverted
    BSPTreeRenderedObject(dim, leftClipped.merge(rightClipped))
  }

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] = Union(a.transformed(f), b.transformed(f))

  override lazy val minBound: Vertex = a.minBound.min(b.minBound)
  override lazy val maxBound: Vertex = a.maxBound.max(b.maxBound)
}

