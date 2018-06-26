package net.joewing.scalacad

case class Polygon(vertices: Seq[Vertex]) {

  lazy val normal: Vertex = (vertices(1) - vertices.head).cross(vertices(2) - vertices.head).unit

  def flip: Polygon = Polygon(vertices.reverse)

  def scaled(x: Double = 1, y: Double = 1, z: Double = 1): Polygon = Polygon(vertices.map(_.scaled(x, y, z)))

  def rotated(x: Double = 0, y: Double = 0, z: Double = 0): Polygon = Polygon(vertices.map(_.rotated(x, y, z)))

  def moved(x: Double = 0, y: Double = 0, z: Double = 0): Polygon = Polygon(vertices.map(_.moved(x, y, z)))

  def edges: Seq[(Vertex, Vertex)] = (vertices :+ vertices.head).sliding(2).map { case Seq(a, b) => a -> b }.toSeq
}
