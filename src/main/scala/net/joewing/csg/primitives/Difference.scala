package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Difference(base: Renderable, minus: Renderable) extends Operation {
  def render: BSPTree = {
    val a = base.render.inverted.clip(minus.render)
    val b = minus.render.clip(a).inverted.clip(a).inverted
    a.merge(b).inverted
  }
}
