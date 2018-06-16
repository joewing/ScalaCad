package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Rotate[D <: Dim](obj: Primitive[D], x: Double = 0, y: Double = 0, z: Double = 0) extends Primitive[D] {
  def render: BSPTree = BSPTree(obj.render.allFacets.map(_.rotated(x, y, z)))
}

