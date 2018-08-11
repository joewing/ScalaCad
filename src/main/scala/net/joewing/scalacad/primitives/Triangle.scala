package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Surface, Vertex}

case class Triangle(base: Double, height: Double) extends Primitive[TwoDimensional] {
  def render: Surface = {
    Surface.fromFacets(Seq(Facet(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(base, height, 0))))
  }
}
