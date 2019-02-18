package net.joewing.scalacad

final case class Polygon3d(vertices: IndexedSeq[Vertex]) {

  lazy val normal: Vertex = {
    val a = vertices.head
    val b = vertices(1)
    val c = vertices(2)
    (b - a).cross(c - a).unit
  }

  lazy val minBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ min _)
  lazy val maxBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ max _)

  def flip: Polygon3d = Polygon3d(vertices.reverse)

  def scaled(x: Double = 1, y: Double = 1, z: Double = 1): Polygon3d = {
    val newPolygon = Polygon3d(vertices.map(_.scaled(x, y, z)))
    if (x * y * z < 0) newPolygon.flip else newPolygon
  }

  def rotated(x: Double = 0, y: Double = 0, z: Double = 0): Polygon3d = Polygon3d(
    vertices.map(_.rotated(x, y, z))
  )

  def moved(x: Double = 0, y: Double = 0, z: Double = 0): Polygon3d = Polygon3d(
    vertices.map(_.moved(x, y, z))
  )
}
