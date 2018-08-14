package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, RenderedObject, Vertex}

case class Cube(width: Double, height: Double, depth: Double) extends Primitive[ThreeDimensional] {
  lazy val render: RenderedObject = {
    val sides = Seq(
      Seq(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(width, height, 0), Vertex(width, 0, 0)),
      Seq(Vertex(width, height, depth), Vertex(0, height, depth), Vertex(0, 0, depth), Vertex(width, 0, depth)),
      Seq(Vertex(width, 0, depth), Vertex(0, 0, depth), Vertex(0, 0, 0), Vertex(width, 0, 0)),
      Seq(Vertex(0, height, 0), Vertex(0, height, depth), Vertex(width, height, depth), Vertex(width, height, 0)),
      Seq(Vertex(0, height, depth), Vertex(0, height, 0), Vertex(0, 0, 0), Vertex(0, 0, depth)),
      Seq(Vertex(width, 0, depth), Vertex(width, 0, 0), Vertex(width, height, 0), Vertex(width, height, depth))
    )
    RenderedObject.fromFacets(sides.flatMap(Facet.fromVertices))
  }
}
