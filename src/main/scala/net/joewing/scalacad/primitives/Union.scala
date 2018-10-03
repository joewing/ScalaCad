package net.joewing.scalacad.primitives

import net.joewing.scalacad._

final case class Union[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  val dim: D = a.dim

  protected def render: RenderedObject = {
    val left = a.rendered.tree
    val right = b.rendered.tree

    val mina = a.minBound
    val maxa = a.maxBound
    val minb = b.minBound
    val maxb = b.maxBound

    if (
      maxa.x < minb.x || maxa.y < minb.y || maxa.z < minb.z ||
      mina.x > maxb.x || mina.y > maxb.y || mina.z > maxb.z
    ) {
      BSPTreeRenderedObject(dim, left.merge(right))
    } else {
      val leftClipped = left.clip(right)
      val rightClipped = right.clip(leftClipped).inverted.clip(leftClipped).inverted
      BSPTreeRenderedObject(dim, leftClipped.merge(rightClipped))
    }
  }

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] = Union(a.transformed(f), b.transformed(f))

  lazy val minBound: Vertex = a.minBound.min(b.minBound)
  lazy val maxBound: Vertex = a.maxBound.max(b.maxBound)
}

