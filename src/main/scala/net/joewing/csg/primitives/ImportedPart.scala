package net.joewing.csg.primitives

import net.joewing.csg.{BSPTree, Polygon}

case class ImportedPart(polygons: Seq[Polygon]) extends Primitive[ThreeDimensional] {
  def render: BSPTree = BSPTree(polygons)
}
