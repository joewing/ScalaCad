package net.joewing.csg.io

import net.joewing.csg.{Polygon, Vertex}

final case class Facet(v1: Vertex, v2: Vertex, v3: Vertex) {

  // Outward normal according to the right-hand rule.
  lazy val normal: Vertex = (v2 - v1).cross(v3 - v1).unit

  lazy val vertices: Seq[Vertex] = Vector(v1, v2, v3)
}

object Facet {
  def fromPolygon(polygon: Polygon): Seq[Facet] = {
    Vector.range(1, polygon.vertices.size - 1).map { i =>
      Facet(polygon.vertices.head, polygon.vertices(i), polygon.vertices(i + 1))
    }
  }

  def fromPolygons(polygons: Seq[Polygon]): Seq[Facet] = polygons.flatMap(fromPolygon)
}
