package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Intersection(a: Renderable, b: Renderable) extends Operation {
  def render: BSPTree = a.render.intersect(b.render)
}
