package net.joewing.csg

final case class Facet(v1: Vertex, v2: Vertex, v3: Vertex) {

  // Outward normal according to the right-hand rule.
  lazy val normal: Vertex = (v2 - v1).cross(v3 - v1).unit

  private def min3(a: Double, b: Double, c: Double): Double =
    math.min(math.min(a, b), c)

  private def max3(a: Double, b: Double, c: Double): Double =
    math.max(math.max(a, b), c)

  lazy val vertices: Seq[Vertex] = Vector(v1, v2, v3)
}
