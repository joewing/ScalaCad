package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Vertex}

object Sphere {
  def apply(r: Double = 1, slicesOverride: Int = 0, stacksOverride: Int = 0): Primitive3d = {

    val fa: Double = 12.0
    val fs: Double = 2.0

    val slices: Int = if (slicesOverride > 0) math.max(slicesOverride, 3) else {
      math.ceil(math.max(math.min(360.0 / fa, r * 2.0 * math.Pi / fs), 5)).toInt
    }

    val stacks: Int = if (stacksOverride > 0) math.max(stacksOverride, 2) else {
      math.ceil(math.max(math.min(360.0 / fa, r * 2.0 * math.Pi / fs), 2)).toInt
    }

    def vertex(x: Double, y: Double): Vertex = {
      val theta = x * 2.0 * math.Pi / slices
      val phi = y * math.Pi / stacks
      Vertex(
        r * math.cos(theta) * math.sin(phi),
        r * math.cos(phi),
        r * math.sin(theta) * math.sin(phi)
      )
    }

    val facets = Vector.tabulate(slices, stacks) { (x, y) =>
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

    Primitive3d(facets)
  }
}
