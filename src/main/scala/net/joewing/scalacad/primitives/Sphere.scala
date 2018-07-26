package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Vertex}

case class Sphere(r: Double = 1, slices: Int = 8, stacks: Int = 8) extends Primitive[ThreeDimensional] {
  require(slices >= 3, s"Need at least 3 slices, got $slices")
  require(stacks >= 2, s"Need at least 2 stacks, got $stacks")

  private def vertex(x: Double, y: Double): Vertex = {
    val theta = x * 2.0 * math.Pi / slices
    val phi = y * math.Pi / stacks
    Vertex(
      r * math.cos(theta) * math.sin(phi),
      r * math.cos(phi),
      r * math.sin(theta) * math.sin(phi)
    )
  }

  def render: Seq[Facet] = {
    Vector.tabulate(slices, stacks) { (x, y) =>
      val v1 = vertex(x, y)
      val v2 = vertex(x + 1, y)
      val v3 = vertex(x + 1, y + 1)
      val v4 = vertex(x, y + 1)
      if (y == 0) {
        Vector(Facet(v1, v3, v4))
      } else if (y == stacks - 1) {
        Vector(Facet(v1, v2, v4))
      } else {
        Facet.fromVertices(Seq(v1, v2, v3, v4))
      }
    }.flatten.flatten
  }
}
