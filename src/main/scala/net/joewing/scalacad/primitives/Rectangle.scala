package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Vertex}

object Rectangle {
  def apply(width: Double, height: Double): Primitive2d = {
    Primitive2d(
      Facet.fromVertices(
        Seq(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(width, height, 0), Vertex(width, 0, 0))
      )
    )
  }
}
