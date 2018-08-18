package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, RenderedObject, Vertex}

case class Triangle(base: Double, height: Double) extends Primitive2d {
  lazy val render: RenderedObject = {
    RenderedObject.fromFacets(Seq(Facet(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(base, height, 0))))
  }
}
