package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Scale[D <: Dim](obj: Primitive[D], x: Double, y: Double, z: Double) extends Primitive[D] {
  def render: BSPTree = BSPTree(obj.render.allFacets.map(_.scaled(x, y, z)))
}

