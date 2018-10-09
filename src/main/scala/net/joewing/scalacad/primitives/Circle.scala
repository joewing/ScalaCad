package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Vertex}

object Circle {
  def apply(r: Double, sidesOverride: Int = 0): Primitive2d = {

    val fa = 12.0
    val fs = 2.0
    val sides: Int = if (sidesOverride > 0) math.max(sidesOverride, 3) else {
      math.ceil(math.max(math.min(360.0 / fa, r * 2.0 * math.Pi / fs), 5)).toInt
    }

    val vs = Vector.tabulate(sides) { i =>
      val angle = -i * math.Pi * 2.0 / sides
      Vertex(r * math.cos(angle), r * math.sin(angle), 0)
    }
    Primitive2d(Facet.fromVertices(vs))
  }
}
