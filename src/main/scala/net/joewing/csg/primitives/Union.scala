package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Union(a: Renderable, b: Renderable) extends Operation {
  def render: BSPTree = a.render.union(b.render)
}
