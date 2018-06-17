package net.joewing.csg.primitives

import net.joewing.csg.{BSPTree, Facet, Vertex}

case class Rectangle(width: Double, height: Double) extends Primitive[TwoDimensional] {
  def render: BSPTree = {
    BSPTree(
      Seq(
        Facet(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(width, height, 0)),
        Facet(Vertex(0, 0, 0), Vertex(width, height, 0), Vertex(width, 0, 0))
      )
    )
  }
}
