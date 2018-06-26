package net.joewing.scalacad.primitives

import net.joewing.scalacad.{BSPTree, Polygon, Vertex}

case class Rectangle(width: Double, height: Double) extends Primitive[TwoDimensional] {
  def render: BSPTree = {
    BSPTree(
      Seq(
        Polygon(
          Seq(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(width, height, 0), Vertex(width, 0, 0))
        )
      )
    )
  }
}
