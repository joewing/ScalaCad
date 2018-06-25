package net.joewing.csg.primitives

import net.joewing.csg.{BSPTree, Polygon, Vertex}

case class Triangle(base: Double, height: Double) extends Primitive[TwoDimensional] {
  def render: BSPTree = {
    BSPTree(Seq(Polygon(Seq(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(base, height, 0)))))
  }
}
