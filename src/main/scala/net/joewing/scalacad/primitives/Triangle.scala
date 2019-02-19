package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Polygon3d, Vertex}

object Triangle {
  def apply(base: Double, height: Double): Primitive2d = {
    Primitive2d(Vector(Polygon3d(Array(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(base, height, 0)))))
  }
}
