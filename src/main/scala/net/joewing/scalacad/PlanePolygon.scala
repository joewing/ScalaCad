package net.joewing.scalacad

case class PlanePolygon(planes: IndexedSeq[Plane]) {

  val support: Plane = planes.head
  val bounding: IndexedSeq[Plane] = planes.tail
  lazy val normal: Vertex = support.normal

  def flip: PlanePolygon = PlanePolygon(support.flip +: bounding.reverse)

  lazy val vertices: Seq[Vertex] = {
    val p1 = support
    val p1n = BigVertex(p1.normal)
    (bounding.last +: bounding).sliding(2).map { case Seq(p2, p3) =>
      // Compute the point of intersection.
      val p2n = BigVertex(p2.normal)
      val p3n = BigVertex(p3.normal)
      val n2xn3 = p2n.cross(p3n)
      val n3xn1 = p3n.cross(p1n)
      val n1xn2 = p1n.cross(p2n)
      val denom = -p1n.dot(n2xn3)
      ((n2xn3 * p1.w + n3xn1 * p2.w + n1xn2 * p3.w) / denom).toVertex
    }.toVector
  }
}

object PlanePolygon {
  def fromFacet(facet: Facet): PlanePolygon = {
    val support = Plane(facet.v1, facet.v2, facet.v3)
    val p1 = Plane(facet.v1, facet.v2, facet.v1 + facet.normal)
    val p2 = Plane(facet.v2, facet.v3, facet.v2 + facet.normal)
    val p3 = Plane(facet.v3, facet.v1, facet.v3 + facet.normal)
    PlanePolygon(Vector(support, p1, p2, p3))
  }

  def fromVertices(vertices: Seq[Vertex]): PlanePolygon = {
    val support = Plane(vertices)
    val planes = (vertices :+ vertices.head).sliding(2).map { case Seq(v1, v2) =>
      // v1 and v2 represent an edge intersecting the support plane.
      // Here we need to compute the plane through v1 and v2 that intersects the support plane.
      // The normal of the support plane will be contained in the plane as will the
      // vector created by the two vertices.
      val n = (v1 - support.normal).cross(v2 - support.normal).unit
      val w = -support.normal.dot(n)
      Plane(n, w)
    }.toIndexedSeq
    PlanePolygon(support +: planes)
  }
}
