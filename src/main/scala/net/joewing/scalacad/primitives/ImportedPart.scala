package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, RenderedObject}

case class ImportedPart(facets: Seq[Facet]) extends Primitive3d {
  lazy val render: RenderedObject = RenderedObject.fromFacets(facets)
}
