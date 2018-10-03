package net.joewing.scalacad

final case class Facet(v1: Vertex, v2: Vertex, v3: Vertex) {

  // Outward normal according to the right-hand rule.
  lazy val normal: Vertex = (v2 - v1).cross(v3 - v1).unit

  //require(!v1.approxEqual(v2) && !v2.approxEqual(v3) && !v3.approxEqual(v1), s"invalid facet: $v1, $v2, $v3")
  //require(normal.length > 0, s"invalid facet: $v1, $v2, $v3, ${v2 - v1} x ${v3 - v1} = ${(v2 - v1).cross(v3 - v1)}")

  lazy val vertices: Seq[Vertex] = Vector(v1, v2, v3)

  lazy val area: Double = (v2 - v1).cross(v3 - v1).length / 2.0

  def minBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ min _)
  def maxBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ max _)

  def centroid: Vertex = v1 / 3 + v2 / 3 + v3 / 3

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

  def contains(p: Vertex, epsilon: Double = Vertex.epsilon): Boolean = {
    val alpha = (v2 - p).cross(v3 - p).length / (area * 2.0)
    val beta = (v3 - p).cross(v1 - p).length / (area * 2.0)
    val gamma = (v1 - p).cross(v2 - p).length / (area * 2.0)
    val residual = 1 - alpha - beta - gamma
    val ep1 = epsilon + 1
    val me = -epsilon
    alpha > me && alpha < ep1 && beta > me && beta < ep1 && gamma > me && gamma < ep1 && math.abs(residual) < epsilon
  }

  def onEdge(p: Vertex, epsilon: Double = Vertex.epsilon): Boolean = {
    p.collinear(v1, v2, epsilon) && p.between(v1, v2) ||
    p.collinear(v2, v3, epsilon) && p.between(v2, v3) ||
    p.collinear(v3, v1, epsilon) && p.between(v3, v1) ||
    p.approxEqual(v1) || p.approxEqual(v2) || p.approxEqual(v3)
  }

  def coplanar(p: Vertex, epsilon: Double = Vertex.epsilon): Boolean = {
    math.abs((v2 - v1).cross(p - v1).dot(v3 - v1)) < epsilon
  }

  def coplanar(other: Facet, epsilon: Double): Boolean = {
    normal.approxEqual(other.normal, epsilon) && coplanar(other.v1, epsilon)
  }
}

object Facet {
  def fromVertices(vertices: Seq[Vertex]): Vector[Facet] = {
    vertices.tail.sliding(2).map { case Seq(a, b) =>
      Facet(vertices.head, a, b)
    }.toVector
  }
}
