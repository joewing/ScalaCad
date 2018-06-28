package net.joewing.scalacad

case class Polygon(vertices: Seq[Vertex]) {

  lazy val normal: Vertex = {
    // Search for a continuous set of three points that are not collinear.
    val count = vertices.size
    (0 until count).view.map { i =>
      val a = vertices(i)
      val b = vertices((i + 1) % count)
      val c = vertices((i + 2) % count)
      (b - a).cross(c - a).unit
    }.find(_.length > 0).getOrElse(Vertex(0, 0, 0))
  }

  def minBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ min _)
  def maxBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ max _)

  def flip: Polygon = Polygon(vertices.reverse)

  def scaled(x: Double = 1, y: Double = 1, z: Double = 1): Polygon = Polygon(vertices.map(_.scaled(x, y, z)))

  def rotated(x: Double = 0, y: Double = 0, z: Double = 0): Polygon = Polygon(vertices.map(_.rotated(x, y, z)))

  def moved(x: Double = 0, y: Double = 0, z: Double = 0): Polygon = Polygon(vertices.map(_.moved(x, y, z)))

  def edges: Seq[(Vertex, Vertex)] = (vertices :+ vertices.head).sliding(2).map { case Seq(a, b) => a -> b }.toSeq
}
