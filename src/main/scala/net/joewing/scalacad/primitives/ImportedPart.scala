package net.joewing.scalacad.primitives

import net.joewing.scalacad.{BSPTree, Polygon}

case class ImportedPart(polygons: Seq[Polygon]) extends Primitive[ThreeDimensional] {
  def render: BSPTree = BSPTree(polygons)
}
