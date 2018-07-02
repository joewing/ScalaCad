package net.joewing.scalacad.io

import net.joewing.scalacad.{Octree, Polygon, Vertex}

final case class Facet(v1: Vertex, v2: Vertex, v3: Vertex) {

  // Outward normal according to the right-hand rule.
  lazy val normal: Vertex = (v2 - v1).cross(v3 - v1).unit

  lazy val vertices: Seq[Vertex] = Vector(v1, v2, v3)

  def flip: Facet = Facet(v3, v2, v1)
}

object Facet {

  /* Get all vertices from polygons such that they are collinear with a and b and between a and b. */
  def containedVertices(a: Vertex, b: Vertex, others: Seq[Vertex]): Seq[Vertex] = {
    others.filter { v =>
      v.collinear(a, b) && v.between(a, b)
    }.toVector.sortBy(_.solve(a, b))
  }

  private def insertVertices12(facet: Facet, others: Seq[Vertex]): Seq[Facet] = {
    val contained = containedVertices(facet.v1, facet.v2, others) :+ facet.v2
    contained.foldLeft((facet.v1, Vector.empty[Facet])) { case ((lastVertex, facets), nextVertex) =>
      if (lastVertex.approxEqual(nextVertex)) {
        (lastVertex, facets)
      } else {
        (nextVertex, facets :+ Facet(lastVertex, nextVertex, facet.v3))
      }
    }._2
  }

  private def insertVertices23(facet: Facet, others: Seq[Vertex]): Seq[Facet] = {
    val contained = containedVertices(facet.v2, facet.v3, others) :+ facet.v3
    contained.foldLeft((facet.v2, Vector.empty[Facet])) { case ((lastVertex, facets), nextVertex) =>
      if (lastVertex.approxEqual(nextVertex)) {
        (lastVertex, facets)
      } else {
        (nextVertex, facets :+ Facet(facet.v1, lastVertex, nextVertex))
      }
    }._2
  }

  private def insertVertices31(facet: Facet, others: Seq[Vertex]): Vector[Facet] = {
    val contained = containedVertices(facet.v3, facet.v1, others) :+ facet.v1
    contained.foldLeft((facet.v3, Vector.empty[Facet])) { case ((lastVertex, facets), nextVertex) =>
      if (lastVertex.approxEqual(nextVertex)) {
        (lastVertex, facets)
      } else {
        (nextVertex, facets :+ Facet(nextVertex, facet.v2, lastVertex))
      }
    }._2
  }

  /** Convert polygons to triangular faces. */
  def fromPolygons(polygons: Seq[Polygon]): Seq[Facet] = {
    val tree = Octree(polygons.flatMap(_.vertices))
    polygons.par.flatMap { polygon =>
      val others = tree.contained(polygon.minBound, polygon.maxBound)
      polygon.vertices.tail.sliding(2).flatMap { case Seq(a, b) =>

        // A triangle from the polygon.
        val facet = Facet(polygon.vertices.head, a, b)

        // Insert extra vertices between v1 and v2, starting at v1 and ending at v2.
        // This creates triangles of the form v1-x-v3, x-y-v3, y-v2-v3
        val with12 = insertVertices12(facet, others)

        // Insert extra vertices between v2 and v3, starting at v2 and ending at v3.
        // Note that this only needs to happen on the last triangle in with12, since it
        // will contain the v2-v3 edge (the v1 edge may be different here).
        // This creates triangles of the form v1'-v2-x, v1'-x-y, v1'-y-v3
        val with23 = with12.init ++ insertVertices23(with12.last, others)

        // Insert extra vertices between v3 and v1.
        // If there are multiple triangles in with12, then this needs to happen on the first of with23, which will
        // contain the v3-v1 edge. If there is only one triangle in with12, then this needs to happen
        // on the last of with23.
        val with31 = if (with12.length == 1) {
          with23.init ++ insertVertices31(with23.last, others)
        } else {
          insertVertices31(with23.head, others) ++ with23.tail
        }

        with31
      }
    }.seq
  }
}
