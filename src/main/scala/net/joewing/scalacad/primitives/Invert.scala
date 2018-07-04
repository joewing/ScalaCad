package net.joewing.scalacad.primitives

import net.joewing.scalacad.{BSPTree, Polygon}

case class Invert[D <: Dim](obj: Primitive[D]) extends Primitive[D] {
  def render: Seq[Polygon] = BSPTree(obj.render).inverted.allPolygons
}

