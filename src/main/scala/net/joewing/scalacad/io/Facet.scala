package net.joewing.scalacad.io

import net.joewing.scalacad.{Octree, Polygon, Vertex}

final case class Facet(v1: Vertex, v2: Vertex, v3: Vertex) {

  // Outward normal according to the right-hand rule.
  lazy val normal: Vertex = (v2 - v1).cross(v3 - v1).unit

  lazy val vertices: Seq[Vertex] = Vector(v1, v2, v3)

  def flip: Facet = Facet(v3, v2, v1)
}

object Facet {

  private val epsilon = 1e-6

  /* Get all vertices from polygons such that they are collinear with a and b and between a and b. */
  def containedVertices(a: Vertex, b: Vertex, others: Seq[Vertex]): Seq[Vertex] = {
    others.filter { v =>
      v.collinear(a, b) && v.between(a, b)
    }.toVector.sortBy(_.solve(a, b))
  }

  private def fix(d: Double): Double = math.round(d / epsilon) * epsilon

  private def insertVertices12(facet: Facet, others: Seq[Vertex]): Seq[Facet] = {
    val contained = containedVertices(facet.v1, facet.v2, others) :+ facet.v2
    contained.foldLeft((facet.v1, Vector.empty[Facet])) { case ((lastVertex, facets), nextVertex) =>
      (nextVertex, facets :+ Facet(lastVertex, nextVertex, facet.v3))
    }._2
  }

  private def insertVertices23(facet: Facet, others: Seq[Vertex]): Seq[Facet] = {
    val contained = containedVertices(facet.v2, facet.v3, others) :+ facet.v3
    contained.foldLeft((facet.v2, Vector.empty[Facet])) { case ((lastVertex, facets), nextVertex) =>
      (nextVertex, facets :+ Facet(facet.v1, lastVertex, nextVertex))
    }._2
  }

  private def insertVertices31(facet: Facet, others: Seq[Vertex]): Vector[Facet] = {
    val contained = containedVertices(facet.v3, facet.v1, others) :+ facet.v1
    contained.foldLeft((facet.v3, Vector.empty[Facet])) { case ((lastVertex, facets), nextVertex) =>
      (nextVertex, facets :+ Facet(nextVertex, facet.v2, lastVertex))
    }._2
  }

  /** Convert polygons to triangular faces. */
  def fromPolygons(polygons: Seq[Polygon]): Seq[Facet] = {
    // First add extra vertices to the polygon so that there is no
    // edge containing a vertex from another polygon.
    // Next, convert the updated polygons to triangles.
    val fixed = polygons.par.map { p =>
      Polygon(p.vertices.map(v => Vertex(fix(v.x1), fix(v.x2), fix(v.x3))))
    }.seq
    val tree = Octree(fixed.flatMap(_.vertices))
    fixed.par.flatMap { polygon =>
      val others = tree.contained(polygon.minBound, polygon.maxBound)
      polygon.vertices.tail.sliding(2).flatMap { case Seq(a, b) =>
        val with23 = insertVertices23(Facet(polygon.vertices.head, a, b), others)
        val with12 = insertVertices12(with23.head, others) ++ with23.tail
        with12.init ++ insertVertices31(with12.last, others)
      }
    }.seq
  }
}
