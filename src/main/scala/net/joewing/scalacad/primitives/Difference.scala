package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Utils}

case class Difference[D <: Dim](base: Primitive[D], minus: Primitive[D]) extends Primitive[D] {
  def render: Seq[Facet] = {

    // Render sub-objects.
    val left = base.render
    val right = minus.render

    // Insert vertices from intersections with the other facets.
    val leftFilled = left.flatMap(f => Utils.insertIntersections(f, right))
    val rightFilled = right.flatMap(f => Utils.insertIntersections(f, left)).map(_.flip)

    val merged = leftFilled ++ rightFilled
    merged.filter { facet =>
      val centroid = facet.centroid
      val containedLeft = Utils.isContained(left, centroid)
      val containedRight = Utils.isContained(right, centroid)
      val boundaryLeft = Utils.onBoundary(left, centroid)
      val boundaryRight = Utils.onBoundary(right, centroid)
        (boundaryLeft && !containedRight && !boundaryRight) || (containedLeft && boundaryRight && !boundaryLeft)
    }
  }
}

