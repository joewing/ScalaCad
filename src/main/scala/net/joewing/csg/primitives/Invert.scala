package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Invert(obj: Renderable) extends Operation {
  def render: BSPTree = obj.render.inverted
}
