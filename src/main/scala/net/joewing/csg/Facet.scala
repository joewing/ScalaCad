package net.joewing.csg

final case class Facet(v1: Vertex, v2: Vertex, v3: Vertex) {

  // Outward normal according to the right-hand rule.
  lazy val normal: Vertex = (v2 - v1).cross(v3 - v1).unit

  private def min3(a: Double, b: Double, c: Double): Double =
    math.min(math.min(a, b), c)

  private def max3(a: Double, b: Double, c: Double): Double =
    math.max(math.max(a, b), c)

  lazy val minBound: Vertex = Vertex(
    min3(v1.x1, v2.x1, v3.x1),
    min3(v1.x2, v2.x2, v3.x2),
    min3(v1.x3, v2.x3, v3.x3)
  )

  lazy val maxBound: Vertex = Vertex(
    max3(v1.x1, v2.x1, v3.x1),
    max3(v1.x2, v2.x2, v3.x2),
    max3(v1.x3, v2.x3, v3.x3)
  )

  lazy val vertices: Seq[Vertex] = Vector(v1, v2, v3)

  def scaled(x: Double = 1, y: Double = 1, z: Double = 1): Facet = Facet(
    v1.scaled(x, y, z),
    v2.scaled(x, y, z),
    v3.scaled(x, y, z)
  )

  def rotated(x: Double = 0, y: Double = 0, z: Double = 0): Facet = Facet(
    v1.rotated(x, y, z),
    v2.rotated(x, y, z),
    v3.rotated(x, y, z)
  )

  def moved(x: Double = 0, y: Double = 0, z: Double = 0): Facet = Facet(
    v1.moved(x, y, z),
    v2.moved(x, y, z),
    v3.moved(x, y, z)
  )

  def flip: Facet = Facet(v1, v3, v2)
}
