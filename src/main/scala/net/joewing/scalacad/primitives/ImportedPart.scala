package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, RenderedObject}

case class ImportedPart(facets: Seq[Facet]) extends Primitive[ThreeDimensional] {
  lazy val render: RenderedObject = RenderedObject.fromFacets(facets)
}
