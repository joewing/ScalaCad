package net.joewing.scalacad

final case class Polygon3d(vertices: Seq[Vertex]) {

  lazy val normal: Vertex = {
    val a = vertices.head
    val b = vertices(1)
    val c = vertices(2)
    (b - a).cross(c - a).unit
  }

  lazy val minBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ min _)
  lazy val maxBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ max _)

  def flip: Polygon3d = Polygon3d(vertices.reverse)
}
