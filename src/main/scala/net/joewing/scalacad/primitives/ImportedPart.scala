package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Surface}

case class ImportedPart(facets: Seq[Facet]) extends Primitive[ThreeDimensional] {
  def render: Surface = Surface.fromFacets(facets)
}
