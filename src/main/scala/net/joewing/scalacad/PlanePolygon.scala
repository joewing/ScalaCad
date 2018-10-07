package net.joewing.scalacad

case class PlanePolygon(planes: IndexedSeq[Plane]) {

  val support: Plane = planes.head
  val bounding: IndexedSeq[Plane] = planes.tail
  lazy val normal: Vertex = support.normal

  def flip: PlanePolygon = PlanePolygon(support.flip +: bounding.reverse)

  lazy val vertices: Seq[Vertex] = {
    val p1 = support
    val p1n = PlanePolygon.toBigVertex(p1.normal)
    (bounding.last +: bounding).sliding(2).toSeq.par.map { case Seq(p2, p3) =>
      // Compute the point of intersection.
      val p2n = PlanePolygon.toBigVertex(p2.normal)
      val p3n = PlanePolygon.toBigVertex(p3.normal)
      val n2xn3 = PlanePolygon.cross(p2n, p3n)
      val n3xn1 = PlanePolygon.cross(p3n, p1n)
      val n1xn2 = PlanePolygon.cross(p1n, p2n)
      val denom = -PlanePolygon.dot(p1n, n2xn3)
      Vertex(
        ((n2xn3._1 * p1.w + n3xn1._1 * p2.w + n1xn2._1 * p3.w) / denom).toDouble,
        ((n2xn3._2 * p1.w + n3xn1._2 * p2.w + n1xn2._2 * p3.w) / denom).toDouble,
        ((n2xn3._3 * p1.w + n3xn1._3 * p2.w + n1xn2._3 * p3.w) / denom).toDouble
      )
    }.seq
  }
}

object PlanePolygon {

  private type BigVertex = (BigDecimal, BigDecimal, BigDecimal)

  private def toBigVertex(v: Vertex): BigVertex = {
    (BigDecimal(v.x), BigDecimal(v.y), BigDecimal(v.z))
  }

  private def cross(a: BigVertex, b: BigVertex): BigVertex = (
    a._2 * b._3 - a._3 * b._2,
    a._3 * b._1 - a._1 * b._3,
    a._1 * b._2 - a._2 * b._1
  )

  private def dot(a: BigVertex, b: BigVertex): BigDecimal = {
    a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  }

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
