package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, FacetRenderedObject, RenderedObject}

final case class Primitive2d(facets: Seq[Facet]) extends Primitive[TwoDimensional] {
  implicit val dim: TwoDimensional = Dim.two
  protected def render: RenderedObject = FacetRenderedObject(dim, facets)
}
