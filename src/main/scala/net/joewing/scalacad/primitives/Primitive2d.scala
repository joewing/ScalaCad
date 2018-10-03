package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, FacetRenderedObject, RenderedObject, Vertex}

final case class Primitive2d(facets: Seq[Facet]) extends Primitive[TwoDimensional] {
  val dim: TwoDimensional = Dim.two

  protected def render: RenderedObject = FacetRenderedObject(dim, facets)

  lazy val minBound: Vertex = reduceVertices(facets.flatMap(_.vertices), _ min _)
  lazy val maxBound: Vertex = reduceVertices(facets.flatMap(_.vertices), _ max _)
}
