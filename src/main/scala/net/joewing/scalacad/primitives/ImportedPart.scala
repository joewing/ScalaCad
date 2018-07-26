package net.joewing.scalacad.primitives

import net.joewing.scalacad.Facet

case class ImportedPart(facets: Seq[Facet]) extends Primitive[ThreeDimensional] {
  def render: Seq[Facet] = facets
}
