package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Vertex}

object Polygon {
  def apply(points: Seq[(Double, Double)]): Primitive2d = {
    require(points.size > 2, s"polygon must have at least 3 points, got ${points.size}")
    Primitive2d(Facet.fromVertices(points.map(p => Vertex(p._1, p._2, 0))))
  }
}
