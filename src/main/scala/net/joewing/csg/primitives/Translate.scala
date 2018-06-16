package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Translate(obj: Renderable, x: Double = 0, y: Double = 0, z: Double = 0) extends Operation {
  def render: BSPTree = BSPTree(obj.render.allFacets.map(_.moved(x, y, z)))
}
