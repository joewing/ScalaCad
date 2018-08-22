package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Vertex}

object Circle {
  def apply(r: Double, sides: Int): Primitive2d = {
    require(sides >= 3, s"sides must be at least 3 (got $sides)")
    val vs = Vector.tabulate(sides) { i =>
      val angle = -i * math.Pi * 2.0 / sides
      Vertex(r * math.cos(angle), r * math.sin(angle), 0)
    }
    Primitive2d(Facet.fromVertices(vs))
  }
}
