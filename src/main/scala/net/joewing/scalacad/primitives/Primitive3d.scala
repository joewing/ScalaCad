package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, FacetRenderedObject, RenderedObject}

final case class Primitive3d(facets: Seq[Facet]) extends Primitive[ThreeDimensional] {
  implicit val dim: ThreeDimensional = Dim.three
  protected def render: RenderedObject = FacetRenderedObject(dim, facets)
}
