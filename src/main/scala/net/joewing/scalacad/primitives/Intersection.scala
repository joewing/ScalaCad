package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Utils}

case class Intersection[D <: Dim](a: Primitive[D], b: Primitive[D]) extends Primitive[D] {
  def render: Seq[Facet] = {

    // Render sub-objects.
    val left = a.render
    val right = b.render

    // Insert vertices from intersections with other facets into each side.
    val leftFilled = left.flatMap(f => Utils.insertIntersections(f, right))
    val rightFilled = right.flatMap(f => Utils.insertIntersections(f, left))

    val leftFiltered = leftFilled.filter { facet =>
      Utils.isContained(right, facet.centroid)
    }
    val rightFiltered = rightFilled.filter { facet =>
      Utils.isContained(left, facet.centroid) && !Utils.onBoundary(left, facet.centroid)
    }

    leftFiltered ++ rightFiltered

    /*
    val merged = leftFilled ++ rightFilled
    merged.filter { facet =>
      val centroid = facet.centroid
      val containedLeft = Utils.isContained(left, centroid)
      val containedRight = Utils.isContained(right, centroid)
      val boundaryLeft = Utils.onBoundary(left, centroid)
      val boundaryRight = Utils.onBoundary(right, centroid)

      (containedLeft && containedRight && boundaryRight) || (containedLeft && containedRight && boundaryLeft)
    }
    */
  }
}

