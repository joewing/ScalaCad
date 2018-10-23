package net.joewing.scalacad

final case class Facet(v1: Vertex, v2: Vertex, v3: Vertex) {

  // Outward normal according to the right-hand rule.
  lazy val normal: Vertex = (v2 - v1).cross(v3 - v1).unit

  lazy val vertices: Vector[Vertex] = Vector(v1, v2, v3)

  lazy val area: Double = (v2 - v1).cross(v3 - v1).length / 2.0

  lazy val minBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ min _)
  lazy val maxBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ max _)

  def flip: Facet = Facet(v1, v3, v2)

  def scaled(x: Double = 1, y: Double = 1, z: Double = 1): Facet = Facet(
    v1.scaled(x, y, z), v2.scaled(x, y, z), v3.scaled(x, y, z)
  )

  def rotated(x: Double = 0, y: Double = 0, z: Double = 0): Facet = Facet(
    v1.rotated(x, y, z), v2.rotated(x, y, z), v3.rotated(x, y, z)
  )

  def moved(x: Double = 0, y: Double = 0, z: Double = 0): Facet = Facet(
    v1.moved(x, y, z), v2.moved(x, y, z), v3.moved(x, y, z)
  )

  def edges: Seq[(Vertex, Vertex)] = Seq(v1 -> v2, v2 -> v3, v3 -> v1)
}

object Facet {
  def fromVertices(vertices: Seq[Vertex]): Vector[Facet] = {
    vertices.tail.sliding(2).map { case Seq(a, b) =>
      Facet(vertices.head, a, b)
    }.toVector
  }
}
