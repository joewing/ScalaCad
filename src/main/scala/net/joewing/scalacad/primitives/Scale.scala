package net.joewing.scalacad.primitives

import net.joewing.scalacad.BSPTree

case class Scale[D <: Dim](obj: Primitive[D], x: Double, y: Double, z: Double) extends Primitive[D] {
  def render: BSPTree = BSPTree(obj.render.allPolygons.map(_.scaled(x, y, z)))
}

