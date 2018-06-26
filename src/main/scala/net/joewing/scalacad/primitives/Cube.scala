package net.joewing.scalacad.primitives

import net.joewing.scalacad.{BSPTree, Polygon, Vertex}

case class Cube(width: Double, height: Double, depth: Double) extends Primitive[ThreeDimensional] {
  def render: BSPTree = {
    val polygons = Seq(
      Polygon(Seq(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(width, height, 0), Vertex(width, 0, 0))),
      Polygon(Seq(Vertex(width, height, depth), Vertex(0, height, depth), Vertex(0, 0, depth), Vertex(width, 0, depth))),
      Polygon(Seq(Vertex(width, 0, depth), Vertex(0, 0, depth), Vertex(0, 0, 0), Vertex(width, 0, 0))),
      Polygon(Seq(Vertex(0, height, 0), Vertex(0, height, depth), Vertex(width, height, depth), Vertex(width, height, 0))),
      Polygon(Seq(Vertex(0, height, depth), Vertex(0, height, 0), Vertex(0, 0, 0), Vertex(0, 0, depth))),
      Polygon(Seq(Vertex(width, 0, depth), Vertex(width, 0, 0), Vertex(width, height, 0), Vertex(width, height, depth)))
    )
    BSPTree(polygons)
  }
}
