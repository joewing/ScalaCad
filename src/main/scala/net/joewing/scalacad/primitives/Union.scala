package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Utils}

final case class Union[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: Seq[Facet] = {

    // Render sub-objects.
    val left = a.render
    val right = b.render

    // Insert vertices from intersections with other facets into each side.
    val leftFilled = left.flatMap(f => Utils.insertIntersections(f, right))
    val rightFilled = right.flatMap(f => Utils.insertIntersections(f, left))

    // Filter out interior facets.
    val leftFiltered = leftFilled.filterNot { facet => Utils.isContained(right, facet.centroid) }
    val rightFiltered = rightFilled.filterNot { facet => Utils.isContained(left, facet.centroid) }

    // Merge the sides.
    rightFiltered ++ leftFiltered
  }
}

