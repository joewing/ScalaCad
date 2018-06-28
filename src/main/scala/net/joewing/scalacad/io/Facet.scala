package net.joewing.scalacad.io

import net.joewing.scalacad.{Octree, Polygon, Vertex}

final case class Facet(v1: Vertex, v2: Vertex, v3: Vertex) {

  // Outward normal according to the right-hand rule.
  lazy val normal: Vertex = (v2 - v1).cross(v3 - v1).unit

  lazy val vertices: Seq[Vertex] = Vector(v1, v2, v3)

  def flip: Facet = Facet(v3, v2, v1)
}

object Facet {

  private val epsilon = Vertex.epsilon

  def onEdge(p: Vertex, a: Vertex, b: Vertex, c: Vertex): Boolean = {
    (p.collinear(a, b) && p.between(a, b)) ||
      (p.collinear(b, c) && p.between(b, c)) ||
      (p.collinear(c, a) && p.between(c, a))
  }

  /** Convert a polyon into triangles. */
  def fromPolygon(polygon: Polygon): Seq[Facet] = {
    // Use the edge clipping algorithm to get triangles.
    // This is a simplified version that only considers multiple vertices on an edge.
    val result = scala.collection.mutable.ArrayBuffer[Facet]()
    val vertices = scala.collection.mutable.ListBuffer[Vertex](polygon.vertices: _*)
    while (vertices.length > 2) {
      val count = vertices.length
      (0 until count).find { a =>
        val b = (a + 1) % count
        val c = (b + 1) % count
        val (va, vb, vc) = (vertices(a), vertices(b), vertices(c))
        !va.collinear(vb, vc) && !(1 until count - 2).exists { p =>
          val vp = vertices((p + c) % count)
          onEdge(vp, va, vb, vc)
        }
      } match {
        case Some(index) =>
          val a = index
          val b = (index + 1) % count
          val c = (index + 2) % count
          val f = Facet(vertices(a), vertices(b), vertices(c))
          /*
          require(f.normal.dot(polygon.normal) >= 0, s"Invalid normal created for $polygon: $f")
          require(f.normal.length > 0)
          */
          result += f
          vertices.remove(b)
        case None =>
          vertices.remove(0)
      }
    }
    result
  }

  private def uniq(vs: Seq[Vertex]): Seq[Vertex] = {
    val result = scala.collection.mutable.ArrayBuffer[Vertex]()
    vs.foreach { v =>
      if (result.isEmpty || (!v.approxEqual(result.last, epsilon) && !v.approxEqual(result.head, epsilon))) {
        result += v
      }
    }
    result
  }

  /* Get all vertices from polygons such that they are collinear with a and b and between a and b. */
  def containedVertices(a: Vertex, b: Vertex, others: Seq[Vertex]): Seq[Vertex] = {
    others.filter { v =>
      v.collinear(a, b) && v.between(a, b)
    }.toVector.sortBy(_.solve(a, b))
  }

  private def fix(d: Double): Double = math.round(d / epsilon) * epsilon

  /** Convert polygons to triangular faces. */
  def fromPolygons(polygons: Seq[Polygon]): Seq[Facet] = {
    // First add extra vertices to the polygon so that there is no
    // edge containing a vertex from another polygon.
    // Next, convert the updated polygons to triangles.
    val fixed = polygons.par.map { p =>
      Polygon(p.vertices.map(v => Vertex(fix(v.x1), fix(v.x2), fix(v.x3))))
    }.seq
    val tree = Octree(fixed.flatMap(_.vertices))
    fixed.par.map { polygon =>
      val others = tree.contained(polygon.minBound, polygon.maxBound)
      val newVertices = (polygon.vertices :+ polygon.vertices.head).sliding(2).flatMap { case Seq(a, b) =>
        a +: containedVertices(a, b, others)
      }.toSeq
      Polygon(uniq(newVertices))
    }.flatMap(fromPolygon).seq
  }
}
