package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

case class Union(objs: Renderable*) extends Operation {
  def render: BSPTree = {
    objs.tail.map(_.render).foldLeft(objs.head.render) { (a, b) =>
      val a1 = a.clip(b)
      val b1 = b.clip(a1).inverted.clip(a1).inverted
      a1.merge(b1)
    }
  }
}
