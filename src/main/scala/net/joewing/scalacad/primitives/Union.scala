package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Utils}

final case class Union[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: Seq[Facet] = {

    // Render sub-objects.
    val left = a.render
    val right = b.render

    // Insert vertices from intersections with other facets into each side.
    val (leftFilled, rightFilled) = Utils.insertIntersections(left, right)

    // Remove facets that are contained or on the boundary of the right side from the left side.
    // We will pick up any necessary facets that are contained on both from the right side.
    val leftFiltered = leftFilled.filterNot { facet => Utils.isContained(right, facet.centroid) }

    // Remove facets that are contained on the left side from the right side.
    // Here we keep boundary facets if the normals both point in the same direction.
    val rightFiltered = rightFilled.filter { facet =>
      val keepBoundary = Utils.findFacet(left, facet.centroid).exists { other =>
        other.normal.dot(facet.normal) > 0
      }
      !Utils.isContained(left, facet.centroid) || keepBoundary
    }

    leftFiltered ++ rightFiltered
  }
}

