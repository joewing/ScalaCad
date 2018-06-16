package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Scale(obj: Renderable, x: Double, y: Double, z: Double) extends Operation {
  def render: BSPTree = BSPTree(obj.render.allFacets.map(_.scaled(x, y, z)))
}
