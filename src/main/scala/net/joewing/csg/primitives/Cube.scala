package net.joewing.csg.primitives

import net.joewing.csg.{BSPTree, Facet, Vertex}

case class Cube(width: Double, height: Double, depth: Double) extends Primitive[ThreeDimensional] {
  def render: BSPTree = {
    val facets = Seq(
      Facet(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(width, height, 0)),
      Facet(Vertex(0, 0, 0), Vertex(width, height, 0), Vertex(width, 0, 0)),
      Facet(Vertex(width, height, depth), Vertex(0, height, depth), Vertex(0, 0, depth)),
      Facet(Vertex(width, 0, depth), Vertex(width, height, depth), Vertex(0, 0, depth)),
      Facet(Vertex(width, 0, depth), Vertex(0, 0, depth), Vertex(width, 0, 0)),
      Facet(Vertex(width, 0, 0), Vertex(0, 0, depth), Vertex(0, 0, 0)),
      Facet(Vertex(width, height, 0), Vertex(0, height, depth), Vertex(width, height, depth)),
      Facet(Vertex(0, height, 0), Vertex(0, height, depth), Vertex(width, height, 0)),
      Facet(Vertex(0, height, depth), Vertex(0, height, 0), Vertex(0, 0, depth)),
      Facet(Vertex(0, 0, depth), Vertex(0, height, 0), Vertex(0, 0, 0)),
      Facet(Vertex(width, 0, depth), Vertex(width, height, 0), Vertex(width, height, depth)),
      Facet(Vertex(width, 0, 0), Vertex(width, height, 0), Vertex(width, 0, depth))
    )
    BSPTree(facets)
  }
}
