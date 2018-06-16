package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Intersection(objs: Renderable*) extends Operation {
  def render: BSPTree = {
    objs.tail.foldLeft(objs.head.render) { (a, b) =>
      val a1 = a.inverted
      val b1 = b.render.clip(a1).inverted
      val a2 = a1.clip(b1)
      val b2 = b1.clip(a2)
      a2.merge(b2).inverted
    }
  }
}
